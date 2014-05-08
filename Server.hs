{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Server 
    ( chdbMasterPort
    , chdbSlavePort
    , chdbServerName
    , initMaster
    , __remoteTable
    ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Control.Monad

import Data.Typeable
import Data.Binary
import qualified Data.HashMap as HM
import Data.List

import System.Directory

import Document
import Messages
import CircularQueue

-- Internal messages, used only by non-client processes (they stay within
-- this module).

-- | Gives information about the current version of a document.
data DocStat = DocStat DocId DocRevision deriving (Typeable, Show)

instance Binary DocStat where
  put (DocStat did ver) = put did >> put ver
  get = do
    did <- get
    ver <- get
    return $ DocStat did ver

-- | Gives imformation about the current version of a document accessible
-- | from a specific Process.
data DocUpdate = DocUpdate DocStat ProcessId deriving (Typeable)

instance Binary DocUpdate where
  put (DocUpdate ds pid) = put ds >> put pid
  get = do
    ds  <- get
    pid <- get
    return $ DocUpdate ds pid
    
-- | Messages for triggering document replication.
data Replicate = SendTo DocId ProcessId
               | Replica Document
               deriving (Typeable)

instance Binary Replicate where
    put (SendTo did pid) = putWord8 0 >> put did >> put pid
    put (Replica doc)    = putWord8 1 >> put doc
    get = do
      n <- getWord8
      case n of
        0 -> do
          did <- get
          pid <- get
          return $ SendTo did pid
        1 -> do
          doc <- get
          return $ Replica doc

-- non-message types.

-- | Defines the result of an attempt to store a document.
data ReqResult = NewVer DocStat | Conflict

-- Constants
chdbMasterPort = "55989"
chdbSlavePort  = "55990"
chdbPath = "/tmp/" -- XXX: temproary.
chdbServerName = "chdbMaster"

getDocList :: IO [DocStat]
getDocList = do
  docs <- fmap (filter (isSuffixOf ".chdb")) $ getDirectoryContents chdbPath
  fmap (map toDocUpdate) $ forM docs (decodeFile . (chdbPath ++))
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

-- TODO: verify that this cannot leave the data in an inconsistent state.
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

-- | NOTE: returns a Process ReqResult in order to be consistent with
-- | the other match functions in receiveWait in slave.
getRequest :: GetDoc -> Process ProcessId
getRequest (GetDoc did response) = spawnLocal handler
    where handler = do
            doc <- liftIO $ getDoc did
            say $ "got " ++ (show did)
            sendChan response $ Just doc -- send the doc to the client.

-- | Handle a PutDoc request.
putRequest :: ProcessId -> PutDoc -> Process ProcessId
putRequest mPid (PutDoc doc resp) = do
  self <- getSelfPid
  spawnLocal $ handler self
    where handler slPid = do
            rslt <- liftIO $ putDoc doc
            case rslt of
              NewVer stat@(DocStat _ rev) -> do
                send mPid (DocUpdate stat slPid)
                sendChan resp (Right rev)
              Conflict -> do
                sendChan resp (Left "document version conflict")

slave :: ProcessId -> Process ()
slave mPid = forever $
  receiveWait [ match (putRequest mPid)
              , match getRequest ]
  
-- | initialize a slave process by collecing the list of documents
-- | it has and sending them to the master. 
initSlave :: ProcessId -> Process ()
initSlave mPid = do
  self  <- getSelfPid
  docls <- liftIO $ getDocList
  say $ "got doc list: " ++ (show docls)
  forM_ docls $ \du -> send mPid (DocUpdate du self)
  say $ "slave started on " ++ (show self)
  slave mPid

stat :: DocId -> IO (Maybe DocStat)
stat did = do
  let fname = docId2File did
  fileExists <- doesFileExist fname
  if fileExists 
  then do
    (Doc _ ver _) <- decodeFile fname
    return $ Just (DocStat did ver)
  else return Nothing

replicator :: (DocStat, ProcessId) -> Process ()
replicator ((DocStat did ver), mPid) = do
  mDocStat <- liftIO $ stat did
  case mDocStat of
    Just (DocStat _ localVer) ->
        if localVer == ver
        then return () -- already up to date
        else if localVer < ver 
             then storeReplica 
             else kill mPid "replication conflict" -- XXX: is this a good idea?
    Nothing -> storeReplica
    where storeReplica = do
                  let fname = docId2File did
                  (sMaybeDoc, rMaybeDoc) <- newChan
                  send mPid $ GetDoc did sMaybeDoc
                  Just doc <- receiveChan rMaybeDoc
                  liftIO $ encodeFile (fname ++ ".tmp") doc
                  liftIO $ renameFile (fname ++ ".tmp") fname

-- remotable needs to come before any use of mkClosure
remotable ['initSlave, 'replicator]

type DocumentIndex = HM.Map DocId (DocRevision, [ProcessId])

-- TODO: is there a better way to prevent duplicate pids in the 
--       pid lists?
updateIndex :: DocumentIndex -> DocUpdate -> DocumentIndex
updateIndex index (DocUpdate (DocStat did ver) pid) = 
    if HM.member did index
    then HM.insertWith insDuplicate did (ver, [pid]) index
    else HM.insert did (ver, [pid]) index
        where insDuplicate (newVer, ps) (oldVer, ps')
                  | newVer > oldVer = (newVer, ps ++ filter (/= (head ps)) ps')
                  | otherwise       = (oldVer, (filter (/= (head ps)) ps')++ ps)

master :: DocumentIndex -> CircularQueue ProcessId -> Process ()
master index slaves =
  receiveWait [ match docUpdate
              , match docGetRequest
              , match docPutRequest ] >>= (uncurry master)
    where docUpdate :: DocUpdate -> 
                       Process (DocumentIndex, CircularQueue ProcessId)
          docUpdate du@(DocUpdate (DocStat did _) _) = do
            masterPid <- getSelfPid
            let index' = updateIndex index du
                (Just (ver, ps)) = HM.lookup did index'
            if ps == [] 
            then spawn (processNodeId . peek $ slaves)
                       ($(mkClosure 'replicator) (DocStat did ver, masterPid))
                   >> return () -- This looks like a bad idea... 12AM, 2 beers
            else mapM_ (flip spawn ($(mkClosure 'replicator) 
                                         (DocStat did ver, masterPid)))
                     . map processNodeId $ ps
            return (index', slaves)

-- TODO: abstract the pattern in the next two functions.          
          docGetRequest :: GetDoc -> 
                           Process (DocumentIndex, CircularQueue ProcessId)
          docGetRequest get@(GetDoc did resp) =
            case HM.lookup did index of
              (Just (_, (p:_))) -> send p get >> return (index, slaves)
              Nothing           -> 
                sendChan resp Nothing         >> return (index, slaves)
          
          docPutRequest :: PutDoc -> 
                           Process (DocumentIndex, CircularQueue ProcessId)
          docPutRequest put@(PutDoc doc resp) =
            case HM.lookup (docId doc) index of
              Just (_, (p:_)) -> send p put >> return (index, slaves)
              Nothing         ->
                let (p, slaves') = next slaves in
                send p put >> return (index, slaves')
           
initMaster :: [NodeId] -> Process ()
initMaster slaves = do
  self <- getSelfPid
  register chdbServerName self
  say $ "starting slaves on: " ++ (show slaves)
  slavePids <- forM slaves $ \nid ->
    spawnLink nid ($(mkClosure 'initSlave) self)
  say "starting master"
  index <- populateIndex HM.empty
  say $ "initial index " ++ (show index)
  spawnLocal $ makeConsistent index self
  master index $ toCircularQueue slavePids
    where populateIndex index = do
            mUpdate <- expectTimeout 200000
            case mUpdate of
              Nothing -> return index
              Just du ->
                  populateIndex $ updateIndex index du
          makeConsistent :: DocumentIndex -> ProcessId -> Process ()
          makeConsistent index mPid = do
            let docList = HM.assocs index
            forM_ docList $ \(did, (ver, (owner:others))) -> 
                if others == [] 
                then send mPid $ DocUpdate (DocStat did ver) owner
                else mapM_ (\nid -> 
                                spawn nid ($(mkClosure 'replicator) 
                                                (DocStat did ver, mPid)))
                         . map processNodeId $ others

