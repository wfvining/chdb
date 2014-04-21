{-# LANGUAGE DeriveDataTypeable #-}
module Server where

import Control.Distributed.Process
import Control.Distributed.Process.Node
--import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLn
import Control.Monad

import Data.Binary
import Data.Typeable
import qualified Data.ByteString as BS

import Document

-- A connection request includes a send port to give the client 
-- a port for sending Ops. (SendPort (SendPort Op))
data ConnRequest = CReq (SendPort (SendPort Op)) deriving (Typeable)

data OpResult = OK | Failure String deriving (Show, Typeable)

data Op = GET  DocId (SendPort Document) -- Get the doc specified by DocId
        | PUT  DocId Document (SendPort OpResult) -- Store the Document under DocId
--        | REV  DocId          -- Get the current version of DocId
--        | VIEW -- View is hard. I need to look at the MapReduce example
          deriving (Typeable)

instance Binary Op where
    put (GET docId rp)     = do {putWord8 0; put docId; put rp}
    put (PUT docId doc rp) = do {putWord8 1; put docId; put doc; put rp}
--    put (REV docId)     = do {putWord8 2; put docId}
--    put (VIEW)          = putWord8 3

    get = do n <- getWord8
             case n of
               0 -> do docId <- get
                       rp <- get
                       return $ GET docId rp
               1 -> do docId <- get
                       doc <- get
                       rp <- get
                       return $ PUT docId doc rp
--               2 -> do { docId <- get; return $ REV docId }
--               3 -> return VIEW

instance Binary ConnRequest where
    put (CReq sp) = put sp
    get           = do {sp <- get; return $ CReq sp}

server :: String -> ReceivePort ConnRequest -> Process ()
server path rPort = forever $ do
  (CReq sOp) <- receiveChan rPort
  handlerSOp <- spawnChannelLocal requestHandler
  sendChan sOp handlerSOp

requestHandler :: ReceivePort Op -> Process ()
requestHandler opIn = do
  op <- receiveChan opIn
  case op of
    (GET docId responseChan) -> sendChan responseChan $ Doc "foo" BS.empty

