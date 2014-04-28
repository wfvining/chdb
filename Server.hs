{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Server 
    ( getServerPid
    , getServerPids
    , startServer 
    , chdbPort
    , __remoteTable
    ) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import qualified Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad

import qualified Data.ByteString as BS
import Data.Typeable
import Data.Binary

import Document
import Messages

-- | An internal message passed from the slaves to the master
--   at startup, or whenever a document changes.
data DocUpdate = DocUpdate DocId DocRevision deriving (Typeable)

instance Binary DocUpdate where
  put (DocUpdate did ver) = put did >> put ver
  get = do
    did <- get
    ver <- get
    return $ DocUpdate did ver

chdbPort = "55989"

-- | get the Pids of all running servers.
getServerPids :: Process [ProcessId]
getServerPids = undefined

-- | get the process id of a running server.
getServerPid :: Process ProcessId
getServerPid = undefined

-- | The main server process.
-- /path/ specifies the path to its data files
server :: String -> Process ()
server path = forever $ do
  newIx <- receiveWait [ match getRequest
                       , match putRequest]--XXX how will this work with Filter?
  server path -- newIx

-- | Handle a document get request
getRequest :: GetDoc -> Process () -- ?? What should be in the Process here?
getRequest (GetDoc docId sp) = undefined
putRequest (PutDoc doc sp) = undefined

-- | Initialize the server, registering it on the local node as 
-- | chdbServer.
initServer :: Process ()
initServer = getSelfPid >>= register "chdbServer" >> server "foo"

startServer :: Process ()
startServer = undefined

-- Will need to be able to spawn servers on remote nodes.
remotable ['startServer]
