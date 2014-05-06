-- | The chdb client.
-- 
-- The client defines a simple interactive shell from which a user can
-- do simple things with simple documents. It is really just for
-- testing/demonstrating.
-- 
-- Will Vining
-- Cinco de Mayo 2014
module Client
       ( clientShell 
       , chdbClientPort )
       where

import System.IO

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

-- | the client runs on its own node, which may or may not host
-- another chdb node, so it needs its own port.
chdbClientPort = "55991"

-- | send a request to the server
request :: [String] -> ProcessId -> Process RequestResult
request ["get", did]                mPid = do
  (sport, rport) <- newChan
  send mPid $ GetDoc did sport
  mdoc <- receiveChan rport
  case mdoc of
    Just doc -> return $ Got doc
    Nothing  -> return Fail
request ["put", did, contents]      mPid = do
  (sport, rport) <- newChan
  send mPid $ PutDoc (mkNewDocument did contents) sport
  rslt <- receiveChan rport
  case rslt of
    Left errorMessage -> (liftIO $ putStrLn errorMessage)     >> return Fail
    Right docRevision -> 
      (liftIO . putStrLn $ "stored " 
       ++ (show did) ++ " at version " ++ (show docRevision)) >> return OK
request ["put", did, ver, contents] mPid = do
  (sport, rport) <- newChan
  send mPid $ PutDoc (mkDocument did ver contents) sport
  rslt <- receiveChan rport
  case rslt of
    Left errorMessage -> (liftIO $ putStrLn errorMessage) >> return Fail
    Right docRevision ->
      (liftIO . putStrLn $ "updated " 
       ++ (show did) ++ " to " ++ (show docRevision))     >> return OK
request _                           _    = (liftIO $ showHelp) >> return Fail

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
            reply <- expectTimeout 200000 -- XXX: not sure what the units are...
            case reply of
              Just (WhereIsReply chdbServerName (Just pid)) -> 
                return $ Just pid
              Just (WhereIsReply chdbServerName Nothing) -> 
                expectWhereisReply
              Nothing -> return Nothing

shell :: [NodeId] -> Process ()
shell nodes = do
  (Just chdbMaster) <- findChdbMaster nodes
  link chdbMaster
  liftIO . putStrLn $ "connected! (master: " ++ (show chdbMaster) ++ ")"
  liftIO $ hSetBuffering stdout NoBuffering
  interactiveShell chdbMaster
    where interactiveShell :: ProcessId -> Process ()
          interactiveShell master = do
            liftIO $ putStr "chdb> "
            command <- fmap words $ liftIO getLine
            case command of
              []    -> interactiveShell master
              (c:_) -> if c == "bye" then return () else do
                rslt <- request command master
                liftIO $ print rslt
                interactiveShell master

clientShell :: String -> IO ()
clientShell host = do
  backend <- initializeBackend host chdbClientPort initRemoteTable
  node <- SLn.newLocalNode backend
  peers <- SLn.findPeers backend 500
  putStrLn "running client process"
  runProcess node (shell peers)
