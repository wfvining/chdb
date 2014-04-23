{-# LANGUAGE DeriveDataTypeable #-}
module Server ( ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
--import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLn
import Control.Monad

import qualified Data.ByteString as BS

import Document
import Messages

-- There are some types that the server needs for internal messages 
-- that should not be visible to other modules...
-- The server needs 

-- | The main server process.
-- /path/ specifies the path to its data files
server :: String -> Process ()
server path = forever $ do
  (Connect clientPid) <- expect
  handlerPid <- spawnLocal requestHandler
  send clientPid handlerPid

-- | spawned to handle requests from clients.
requestHandler :: Process ()
requestHandler =
  receiveWait [match getRequest, match putRequest] >> requestHandler
    where getRequest (GetDoc docId sp) = do
            doc <- liftIO $ readDoc "NONONO!" docId
            sendChan sp (Just doc) -- XXX:
          putRequest (PutDoc doc sp)= do
            -- TODO putDoc 
            sendChan sp Nothing

