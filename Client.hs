module Client where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Monad (forever)

import Messages
import Document

makeRequest :: String -> Process ()
makeRequest reqStr = do
  selfNode <- getLocalNode
  let (request:args) = words reqStr
  case request of
    "get" -> 
              

dispatch :: String -> IO ()
dispatch input = do
  let com = head . words $ input
  return ()

main :: IO ()
main = forever $ do
         line <- getLine
         dispatch line
         runProcess makeRequest
         
