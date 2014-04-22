{-# LANGUAGE DeriveDataTypeable #-}
-- | Defines the types used for messages in chdb. 
--   All exported types are @Serializable@ as they will be sent as messages.
module Messages
       ( Connect(..)
       , GetDoc(..)
       , PutDoc(..)
       ) where

import Control.Distributed.Process
  ( SendPort(..)
  , ReceivePort(..)
  , ProcessId(..)
  )
import Data.Binary
import Data.Typeable

import Document

-- | The initial message to establish a connection between a client and a server
data Connect = Connect ProcessId deriving (Typeable)

-- | Messages related to Documents
data GetDoc = GetDoc DocId (SendPort (Maybe Document)) deriving (Typeable)
data PutDoc = PutDoc Document (SendPort (Maybe DocRevision)) deriving Typeable

-- | Compute a view across all data (MapReduce style?)
-- data Filter = Filter (Closure (Document -> Bool)) (SendPort 

instance Binary Connect where
  put (Connect pid) = put pid
  get = do
    pid <- get
    return (Connect pid)

instance Binary GetDoc where
  put (GetDoc dID sp) = do
    put dID
    put sp
  get = do
    dID <- get
    sp  <- get
    return (GetDoc dID sp)
    
instance Binary PutDoc where
  put (PutDoc doc sp) = do
    put doc
    put sp
  get = do
    doc <- get
    sp  <- get
    return (PutDoc doc sp)
