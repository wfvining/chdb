{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Server ( ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
--import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLn
import Control.Monad

import qualified Data.ByteString as BS

import Document
import Messages

-- There are some types that the server needs for internal messages 
-- that should not be visible to other modules...
-- The server needs

data OpResult = IxChange 

-- | The main server process.
-- /path/ specifies the path to its data files
server :: String -> Process ()
server path = forever $ do
  newIx <- receiveWait [match getRequest, match putRequest]
  server path -- newIx

-- | Handle a document get request
getRequest :: GetDoc -> Process () -- ?? What should be in the Process here?
getRequest (GetDoc docId sp) = undefined
putRequest (PutDoc doc sp) = undefined

-- Will need to be able to spawn servers on remote nodes.
remotable ['server]
