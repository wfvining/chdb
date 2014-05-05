module Main where

import System.Environment

import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet

import Server

chdbRemoteTable = Server.__remoteTable initRemoteTable

dispatch :: [String] -> IO ()
-- set up a slave node that will wait for the master to spawn a process on it.
dispatch ["slave", host]  = 
  initializeBackend host chdbSlavePort chdbRemoteTable >>= startSlave
dispatch ["master", host] = do
  backend <- initializeBackend host chdbMasterPort chdbRemoteTable
  startMaster backend initMaster
  putStrLn "Starting server"
  -- todo, find slave NodeIds, spawn Server.master.
dispatch [""] = do
  putStrLn "starting chdb client"
  -- Run a client.

main :: IO ()
main = do 
  args <- getArgs 
  dispatch args
