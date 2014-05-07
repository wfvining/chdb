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
  self       <- getSelfPid
  handlerPid <- spawnLocal $ handler self
--  link handlerPid
  return handlerPid
    where handler slPid = do
            rslt <- liftIO $ putDoc doc
            case rslt of
              NewVer stat@(DocStat _ rev) -> do
                send mPid (DocUpdate stat slPid)
                sendChan resp (Right rev)
--                unlink slPid -- XXX
              Conflict -> do
                sendChan resp (Left "document version conflict")
--                unlink slPid -- XXX

-- | Handle a document replication request.
--   TODO: There is no confirmation that the replication succeeded, might 
--   want to implement that.
replicateDoc :: Replicate -> Process ProcessId
replicateDoc (SendTo did pid) = do
  self <- getSelfPid
  hPid <- spawnLocal sendHandler
  return hPid
    where sendHandler = do
            doc <- liftIO $ getDoc did -- XXX: Should I use a bang pattern here?
            send pid (Replica doc)
replicateDoc (Replica doc) = do
  self <- getSelfPid
  pid  <- spawnLocal receiveHandler
  return pid
    where receiveHandler = do
            rslt <- liftIO $ putDoc doc
            case rslt of
              NewVer _ -> return ()
              Conflict -> die "replication conflict."

slave :: ProcessId -> Process ()
slave mPid = forever $
  receiveWait [ match (putRequest mPid)
              , match getRequest 
              , match replicateDoc ]
  
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

-- remotable needs to come before any use of mkClosure
remotable ['initSlave]

type DocumentIndex = HM.Map DocId (DocRevision, [ProcessId])

updateIndex :: DocumentIndex -> DocUpdate -> DocumentIndex
updateIndex index (DocUpdate (DocStat did ver) pid) = 
    if HM.member did index
    then HM.insertWith insDuplicate did (ver, [pid]) index
    else HM.insert did (ver, [pid]) index
        where insDuplicate (newVer, ps) (oldVer, ps')
                  | newVer > oldVer = (newVer, ps ++ ps')
                  | otherwise       = (oldVer, ps' ++ ps)

master :: DocumentIndex -> CircularQueue ProcessId -> Process ()
master index slaves =
  receiveWait [ match docUpdate
              , match docGetRequest
              , match docPutRequest ] >>= (uncurry master)
    where docUpdate :: DocUpdate -> 
                       Process (DocumentIndex, CircularQueue ProcessId)
          docUpdate du  = return ((updateIndex index du), slaves)
          
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
  master index $ toCircularQueue slavePids
    where populateIndex index = do
            mUpdate <- expectTimeout 200000
            case mUpdate of
              Nothing   -> return index
              (Just du) ->
                  populateIndex $ updateIndex index du
                      
