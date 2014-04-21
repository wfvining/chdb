{-# LANGUAGE DeriveDataTypeable #-}
module Document (DocId, Document(..)) where 

import Data.ByteString
import Data.Typeable
import Data.Binary

type DocId = String
data Document = Doc {docId :: DocId, contents :: ByteString}
                deriving (Show, Typeable)

-- A Document needs to be serializable.
instance Binary Document where
    put (Doc id contents) = do { put id; put contents }
    get = do {id <- get; contents <- get; return $ Doc id contents }
