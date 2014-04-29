{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Server 
    ( chdbPort
    , chdbServerName
    , __remoteTable
    ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Monad

import qualified Data.ByteString as BS
import Data.Typeable
import Data.Binary
import qualified Data.HashMap as HM
import Data.List

import System.Directory

import Document
import Messages

data DocStat = DocStat DocId DocRevision deriving (Typeable)

instance Binary DocStat where
  put (DocStat did ver) = put did >> put ver
  get = do
    did <- get
    ver <- get
    return $ DocStat did ver

data DocUpdate = DocUpdate DocStat ProcessId deriving (Typeable)

instance Binary DocUpdate where
  put (DocUpdate ds pid) = put ds >> put pid
  get = do
    ds  <- get
    pid <- get
    return $ DocUpdate ds pid
    
data ReqResult = ReqDoc Document | OK | ERROR

chdbPort = "55989"
chdbPath = "/tmp/" -- Just temproary.
chdbServerName = "chdbMaster"

getDocList :: IO [DocStat]
getDocList = do
  dirc <- getDirectoryContents chdbPath
  let docs = filter (isSuffixOf ".chdb") dirc
  fmap (map toDocUpdate) $ forM docs decodeFile
  where toDocUpdate :: Document -> DocStat
        toDocUpdate doc = DocStat (docId doc) (revision doc)

getDoc :: DocId -> IO Document
getDoc did = do
  let fname = did ++ ".chdb"
  decodeFile fname
  

master :: HM.Map DocId (DocRevision, ProcessId) -> Process ()
master index = undefined

-- | NOTE: returns a Process ReqResult in order to be consistent with
-- | the other match functions in receiveWait in slave.
getRequest :: GetDoc -> Process ReqResult
getRequest (GetDoc did response) = spawnLocal handler >> return OK
    where handler = do
            doc <- liftIO $ getDoc did
            sendChan response $ Just doc -- send the doc to the client.

-- TODO : spawnLocal a process to handle a PutDoc request
putRequest :: PutDoc -> Process ReqResult
putRequest = undefined

replicateDoc :: DocUpdate -> Process ReqResult
replicateDoc = undefined

slave :: ProcessId -> Process ()
slave mPid = forever $ do
  rslt <- receiveWait [ match putRequest
                      , match getRequest 
                      , match replicateDoc ]
  case rslt of
    OK    -> slave mPid
    ERROR -> die "unknown exception" -- We really should never get here.
    
  

-- | initialize a slave process by collecing the list of documents
-- | it has and sending them to the master. 
initSlave :: ProcessId -> Process ()
initSlave mPid = do
  self  <- getSelfPid
  docls <- liftIO $ getDocList
  forM_ docls $ \du -> send mPid (DocUpdate du self)
  slave mPid

remotable ['initSlave] -- XXX: Does slave need to be remotable?

initMaster :: [NodeId] -> Process ()
initMaster slaves = do
  self <- getSelfPid
  slavePids <- forM slaves $ \nid ->
    spawnLink nid ($(mkClosure 'initSlave) self) 
  master HM.empty

