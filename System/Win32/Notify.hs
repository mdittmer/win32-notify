module System.Win32.Notify (
      watchDirectory        -- FilePath -> Bool -> [EventVariety] -> IO [Event]
    , Event(..)
    , EventVariety(..)

    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Bits
import Data.List (intersect)
import Data.Map (Map)
import System.Directory
import System.Win32.File
import System.Win32.FileNotify
import qualified Data.Map as Map


-- import System.IO.Unsafe (unsafeInterleaveIO)

{-
addDirWatch :: FilePath -> Bool -> [EventVariety] -> (Event -> IO ()) -> IO ()
addDirWatch dir wst evs cb = trace "addDirWatch" (forkIO loop) >> trace "addDirWatch: forked" (return ())
  where loop = do
            trace "addDirWatch: start loop" $ return ()
            e <- watchDirectory dir wst evs
            trace "addDirWatch: watchDirectory returned" $ return ()
            forkIO $ cb e
            trace "addDirWatch: callback forked" $ return ()
-}

data EventVariety
    = Modify
    | Move
    | Create
    | Delete
  deriving Eq

data Event
    -- | A file was modified. @Modified isDirectory file@
    = Modified
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was moved within the directory.
    | Renamed
        { isDirectory :: Bool
        , oldName     :: Maybe FilePath
        , newName     :: FilePath
        }
    -- | A file was created. @Created isDirectory file@
    | Created
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    deriving (Eq, Show)

type Handler = Event -> IO ()

data WatchId = WatchId ThreadId ThreadId
type WatchMap = Map WatchId Handler
data WatchManager = WatchManager (MVar WatchMap)

initWatchManager :: IO WatchManager
initWatchManager = return (newMVar Map.empty :: MVar WatchMap)

killWatchManager :: WatchManager -> IO ()
killWatchManager (WatchManager watchMap) = do
  flip mapM_ (Map.elems watchMap) $ killThreads
  where
    killThreads (tid1, tid2) = do
      killThread tid1
      killThread tid2

varietiesToFnFlags :: [EventVariety] -> FileNotificationFlag
varietiesToFnFlags = foldl (.|.) 0 . map evToFnFlag'
    where evToFnFlag' :: EventVariety -> FileNotificationFlag
          evToFnFlag' ev = case ev of
                            Modify  -> fILE_NOTIFY_CHANGE_LAST_WRITE
                            Move    -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME
                            Create  -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME
                            Delete  -> fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME

-- watchDirectoryOnce :: FilePath -> Bool -> [EventVariety] -> IO
-- watchDirectoryOnce dir wst evs = do
--         h <- getWatchHandle dir
--         readDirectoryChanges h wst (evToFnFlag evs) >>= actsToEvent

watchDirectory :: WatchManager -> FilePath -> Bool -> [EventVariety] -> Handler -> IO ()
watchDirectory (WatchManager watchMap) dir watchSubTree varieties handler = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid1 <- forkIO $ dispatcher chanEvents
  tid2 <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ watchMap $ \m -> return (Map.insert (tid1, tid2) handler watchMap)
  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = do
      events <- readChan chanEvents
      mapM_ maybeHandle events
      dispatcher chanEvents
    osEventsReader :: handleType -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = do
      event <- (readDirectoryChanges watchHandle watchSubTree (varietiesToFnFlags varieties) >>= actsToEvent)
      writeChan chanEvents [event]
      osEventsReader watchHandle chanEvents
    maybeHandle :: Handler
    maybeHandle event =
      if not (null ((eventToVarieties event) `intersect` varieties)) then handler event else return ()
    -- maybeHandle event@(Created  _ _) = handleWhen Create varieties handler event
    -- maybeHandle event@(Deleted  _ _) = if Delete `elem` varieties then handler event else return ()
    -- maybeHandle event@(Modified _ _) = if Modify `elem` varieties then handler event else return ()
    -- maybeHandle event@(Renamed  _ _) = if Move   `elem` varieties then handler event else return ()
    -- handleWhen :: EventVariety -> [EventVariety] -> Handler -> Event -> IO ()
    -- handleWhen variety varieties handler event = if variety `elem` varieties then handler event else return ()

eventToVarieties :: Event -> [EventVariety]
eventToVarieties event = case event of
  Created  _ _   -> [Create]
  Deleted  _ _   -> [Delete]
  Modified _ _   -> [Modify]
  Renamed  _ _ _ -> [Move]

-- watchDirectory :: FilePath -> Bool -> [EventVariety] -> IO [Event]
-- watchDirectory dir wst evs = do
--         h <- getWatchHandle dir
--         loop h
--     where loop h = do e <- readDirectoryChanges h wst (evToFnFlag evs) >>= actsToEvent
--                       es <- unsafeInterleaveIO $ loop h
--                       return $ e:es

actsToEvent :: [(Action, String)] -> IO Event
actsToEvent [] = error "The impossible happened - there was no event!"
actsToEvent [(act, fn)] = do
    isDir <- doesDirectoryExist fn
    case act of
        FileModified    -> return $ Modified isDir (Just fn)
        FileAdded       -> return $ Created isDir fn
        FileRemoved     -> return $ Deleted isDir fn
        FileRenamedOld  -> return $ Renamed isDir Nothing fn
actsToEvent [(FileRenamedOld, fnold),(FileRenamedNew, fnnew)] = do
    isDir <- doesDirectoryExist fnnew
    return $ Renamed isDir (Just fnold) fnnew
