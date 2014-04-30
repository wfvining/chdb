{-# LANGUAGE DeriveDataTypeable #-}
module Document 
    ( DocId
    , DocRevision
    , Document(..)
    , readDoc
    , writeDoc 
    ) where 

import qualified Data.ByteString as BS
import Data.Typeable
import Data.Binary

type DocId = String
type DocRevision = Int

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

-- | Write a @Document@ to file.
readDoc :: String -> DocId -> IO Document
readDoc path dId = decodeFile (path ++ dId)

-- | Read a @Document@ from file.
writeDoc :: String -> Document -> IO ()
writeDoc path doc@(Doc docId _ _) = encodeFile (path ++ docId) doc
