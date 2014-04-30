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
    
data ReqResult = NewVer DocStat | Conflict | OK | ERROR

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

docId2File :: DocId -> FilePath
docId2File did = chdbPath ++ did ++ ".chdb"

-- | Read a chdb document from file.
getDoc :: DocId -> IO Document
getDoc = decodeFile . docId2File

-- | Store a new document.
putNewDoc :: Document -> IO ReqResult
putNewDoc (Doc did _ contents) = do
  encodeFile (docId2File did) (Doc did newDocVersion contents)
  return $ NewVer (DocStat did newDocVersion)
    where newDocVersion = 1

-- TODO: verify that this cannot leav the data in an inconsistenyt state.
updateDoc :: Document -> IO ReqResult
updateDoc (Doc did ver contents) = do
  (Doc _ ver' _) <- decodeFile $ docId2File did
  if ver' /= ver 
    then return Conflict 
    else do 
         let tmpName = (docId2File did) ++ ".tmp"
             newVer  = succ ver
         encodeFile tmpName (Doc did newVer contents)
         renameFile tmpName $ docId2File did            
         return $ NewVer (DocStat did newVer)

doesDocExist :: Document -> IO Bool
doesDocExist (Doc did _ _) = doesFileExist $ docId2File did

-- | handle a PutDoc request.
putDoc :: Document -> IO ReqResult
putDoc doc = do
  fileExists <- doesDocExist doc
  if fileExists then updateDoc doc else putNewDoc doc

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
putRequest (PutDoc doc response) = undefined -- Should the slave send NewVer directly to the master??? I think yes. also, maybe make Process ReqResult a Process ()

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

