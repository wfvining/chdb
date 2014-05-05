-- | The chdb client.
-- 
-- The client defines a simple interactive shell from which a user can
-- do simple things with simple documents. It is really just for
-- testing/demonstrating.
module Client
       ( clientShell 
       , chdbClientPort )
       where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet as SLn

import Control.Monad (forever, forM_)

import qualified Data.ByteString.Char8 as C8

import Messages
import Document
import Server

-- | the possible results of any client request.
data RequestResult = OK | Fail | Got Document

instance Show RequestResult where
  show OK = "ok"
  show Fail = "fail"
  show (Got doc) = show doc

chdbClientPort = "55991"

getServerPid = undefined

get :: Process (Maybe Document)
get = undefined

put :: Process ()
put = undefined

-- | request sends a request to the server
{-request :: [String] -> IO ()
request ("get":args) = do
  backend <- initializeBackend "127.0.0.1" chdbSlavePort chdbRemoteTable
  forkProcess 
-}

request :: [String] -> ProcessId -> Process RequestResult
request ["get", docId] mPid = undefined
request ("put":args) mPid
  | length args == 2 = undefined -- attempt to store a new document.
  | length args == 3 = undefined -- attempt to update an existing document.
  | otherwise = liftIO $ showHelp >> return Fail

showHelp :: IO ()
showHelp = do
  putStrLn "type \'bye\' to exit."
  putStrLn "type \'get <docId>\' to retrieve a document"
  putStrLn "type \'put <docId> [docVersion] <string>\' to store the string under docId. docVersion is required if updateing the document."

-- | send a whereis request to every node in the list, waiting for a response
--   if no response is received within a reasonable time then 
findChdbMaster :: [NodeId] -> Process (Maybe ProcessId)
findChdbMaster nodes = do
  forM_ nodes $ \nid -> whereisRemoteAsync nid chdbServerName
  expectWhereisReply
    where expectWhereisReply = do
            reply <- expectTimeout 2000 -- XXX: not sure what the units are...
            case reply of
              Just (WhereIsReply chdbServerName (Just pid)) -> 
                return $ Just pid
              Just (WhereIsReply chdbServerName Nothing) -> 
                expectWhereisReply
              Nothing -> return Nothing

shell :: [NodeId] -> Process ()
shell nodes = do
  (Just chdbMaster) <- findChdbMaster nodes
  interactiveShell chdbMaster
    where interactiveShell :: ProcessId -> Process ()
          interactiveShell master = do
            liftIO $ putStr "chdb> "
            command@(c:_) <- fmap words $ liftIO getLine
            if c == "bye" then return () else do
              rslt <- request command master
              liftIO $ print rslt
              interactiveShell master

clientShell :: String -> IO ()
clientShell host = do
  backend <- initializeBackend host chdbClientPort initRemoteTable
  node <- SLn.newLocalNode backend
  peers <- SLn.findPeers backend 500
  runProcess node (shell peers)
              
