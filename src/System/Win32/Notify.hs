module System.Win32.Notify
  ( initWatchManager
  , killWatchManager
  , watchDirectory
  , watch
  , Event(..)
  , EventVariety(..)
  , Handler
  , WatchId(..)
  , WatchManager(..)
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

data WatchId = WatchId ThreadId ThreadId deriving (Eq, Ord, Show)
type WatchMap = Map WatchId Handler
data WatchManager = WatchManager (MVar WatchMap)


initWatchManager :: IO WatchManager
initWatchManager =  do
  mvarMap <- newMVar Map.empty
  return (WatchManager mvarMap)

killWatchManager :: WatchManager -> IO ()
killWatchManager (WatchManager mvarMap) = do
  watchMap <- readMVar mvarMap
  flip mapM_ (Map.keys watchMap) $ killThreads
  where
    killThreads :: WatchId -> IO ()
    killThreads (WatchId tid1 tid2)
      | tid1 == tid2 = killThread tid1
      | otherwise    = do
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

watchDirectory :: WatchManager -> FilePath -> Bool -> [EventVariety] -> Handler -> IO WatchId
watchDirectory (WatchManager mvarMap) dir watchSubTree varieties handler = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid1 <- forkIO $ dispatcher chanEvents
  tid2 <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid1 tid2) handler watchMap)
  return (WatchId tid1 tid2)
  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = do
      events <- readChan chanEvents
      mapM_ maybeHandle events
      dispatcher chanEvents
    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = do
      event <- (readDirectoryChanges watchHandle watchSubTree (varietiesToFnFlags varieties) >>= actsToEvent)
      writeChan chanEvents [event]
      osEventsReader watchHandle chanEvents
    maybeHandle :: Handler
    maybeHandle event =
      if not (null ((eventToVarieties event) `intersect` varieties)) then handler event else return ()

watch :: WatchManager -> FilePath -> Bool -> [EventVariety] -> IO (WatchId, Chan Event)
watch (WatchManager mvarMap) dir watchSubTree varieties = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid tid) (\_ -> return ()) watchMap)
  return ((WatchId tid tid), chanEvents)
  where
    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = do
      event <- (readDirectoryChanges watchHandle watchSubTree (varietiesToFnFlags varieties) >>= actsToEvent)
      writeChan chanEvents [event]
      osEventsReader watchHandle chanEvents

eventToVarieties :: Event -> [EventVariety]
eventToVarieties event = case event of
  Created  _ _   -> [Create]
  Deleted  _ _   -> [Delete]
  Modified _ _   -> [Modify]
  Renamed  _ _ _ -> [Move]

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
