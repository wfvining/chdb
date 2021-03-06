{-# LANGUAGE DeriveDataTypeable #-}
-- | Defines the types used for messages in chdb. 
--   All exported types are @Serializable@ as they will be sent as messages.
module Messages
       ( GetDoc(..)
       , PutDoc(..)
       ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Data.Typeable

import Document

-- | Messages related to Documents
data GetDoc = GetDoc DocId (SendPort (Maybe Document)) deriving (Typeable)
data PutDoc = PutDoc Document (SendPort (Either String DocRevision)) 
            deriving Typeable

-- | Compute a view across all data (MapReduce style?)
-- NOTE: The way that closure is used here may not be correct.
data Filter = Filter (Closure (Document -> Bool)) (SendPort [DocId])
            deriving (Typeable)

instance Binary Filter where
    put (Filter clo sp) = do
      put clo
      put sp
    get = do
      clo <- get
      sp  <- get
      return (Filter clo sp)

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
