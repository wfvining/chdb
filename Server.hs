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

-- | An internal message passed from the slaves to the master
--   at startup, or whenever a document changes.
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

master :: HM.Map DocId (DocRevision, ProcessId) -> Process ()
master index = undefined

slave :: ProcessId -> Process ()
slave mPid = forever $ do
  n <- expect :: Process Int -- Just filling space
  liftIO $ print n

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

