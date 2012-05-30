module Main where

import System.IO
import System.Directory
import System.Win32.Notify

main :: IO ()
main = do
  watchManager <- initWatchManager
  home <- getHomeDirectory
  wd <- watchDirectory watchManager home isRecursive varieties handler
  print wd
  putStrLn "Listens to your home directory. Hit enter to terminate."
  getLine -- TODO: This hangs... why is that?
  killWatchManager watchManager
  where
    isRecursive = False
    varieties = [Create, Delete, Modify]
    handler :: Handler
    handler event = putStrLn $ show event
