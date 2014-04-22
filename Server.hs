{-# LANGUAGE DeriveDataTypeable #-}
module Server where

import Control.Distributed.Process
import Control.Distributed.Process.Node
--import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLn
import Control.Monad

import qualified Data.ByteString as BS

import Document
import Messages

server :: String -> Process ()
server path = forever $ do
  (Connect pid) <- expect
  rPid <- spawnLocal requestHandler
  send pid rPid

requestHandler :: Process ()
requestHandler =
  receiveWait [match getRequest, match putRequest] >> requestHandler
    where getRequest (GetDoc docId sp) = do
            -- get the document!
            sendChan sp (Just $ Doc docId 0 BS.empty)
          putRequest (PutDoc doc sp)= do
            -- TODO putDoc 
            sendChan sp Nothing

