module Client where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Monad (forever)

import Messages
import Document
import Server

-- XXX: Maybe this should be in a different module.
chdbRemoteTable = initRemoteTable Server.__remoteTable

-- XXX: 
get :: Process ()
get = do
  srvr <- getServerPid
  ref  <- monitor srvr
  -- ...
  unmonitor ref

-- | request sends a request to the server
request :: [String] -> IO ()
request ("get":args) = do
  backend <- initializeBackend "127.0.0.1" chdbPort chdbRemoteTable
  forkProcess 

showHelp :: IO ()
showHelp = putStrLn "type \'bye\' to exit."

clientShell :: IO ()
clientShell = do
  putStr "chdb> "
  c@(command:_) <- fmap words getLine
  case command of
    "bye"  -> return ()
    "help" -> showHelp >> clientShell
    _      -> request c >> clientShell
              


         
