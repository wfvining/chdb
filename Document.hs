{-# LANGUAGE DeriveDataTypeable #-}
module Document (DocId, DocRevision, Document(..)) where 

import qualified Data.ByteString as BS
import Data.Typeable
import Data.Binary

type DocId = String
type DocRevision = Integer

data Document = Doc { docId :: DocId
                    , revision :: DocRevision
                    , contents :: BS.ByteString}
              deriving (Show, Typeable)

-- A Document needs to be serializable.
instance Binary Document where
    put (Doc docId rev contents) = do { put docId; put rev; put contents }
    get = do
      id <- get 
      rev <- get
      contents <- get
      return (Doc id rev contents)
