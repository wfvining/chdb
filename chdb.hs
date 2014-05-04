module Main where

import System.Environment

import Control.Distributed.Process.Node
import Control.Distriubted.Process.Backend.SimpleLocalnet

import Server

nodes = ["127.0.0.1"]

dispatch :: String -> IO ()
dispatch "server" = do
  putStrLn "Starting server"
  -- Initialize nodes
  backend <- initializeBackend "127.0.0.1" chdbPort $ 
             initRemotetable Server.__remoteTable
  nodes <- get
  forkProcess (initMaster slaveNodes)
dispatch "" = do
  putStrLn "starting chdb client"
  -- Run a client.

main :: IO ()
main = do
  [arg] <- getArgs
  dispatch arg
