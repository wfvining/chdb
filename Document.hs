{-# LANGUAGE DeriveDataTypeable #-}
module Document 
    ( DocId
    , DocRevision
    , Document(..)
    , mkNewDocument
    , mkDocument
    ) where 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
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

mkDocument :: String -> String -> String -> Document
mkDocument sDid sVersion sContents = Doc { docId    = sDid
                                         , revision = read sVersion
                                         , contents = C8.pack sContents }

mkNewDocument :: String -> String -> Document
mkNewDocument sDid sContents = Doc { docId    = sDid
                                   , revision = newDocRevision
                                   , contents = C8.pack sContents }
  -- This must be less than 1, or there could be unhandled conflicts.
  where newDocRevision = -1
