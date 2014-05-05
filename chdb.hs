-- | The amazing CloudHaskell Database!
-- The main driver for chdb.
-- To start chdb, fisrst start slave nodes by running
-- 
-- > ./chdb slave <ip address to listen on>
-- 
-- on each computer to be used as a slave. A single machine can host
-- both a slave and a master node.
-- 
-- Once all slave nodes have been started, start chdb by running
-- 
-- > ./chdb master <ip address to listen on>
-- 
-- This will start slave processes on all the slave nodes, as well as
-- the mater process on the node you execute it on.
-- 
-- Will Vining
-- Cinco de Mayo 2014
module Main where

import System.Environment

import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet

import Server
import Client

chdbRemoteTable = Server.__remoteTable initRemoteTable

dispatch :: [String] -> IO ()
-- set up a slave node that will wait for the master to spawn a process on it.
dispatch ["slave", host]  = 
  initializeBackend host chdbSlavePort chdbRemoteTable >>= startSlave
dispatch ["master", host] = do
  putStrLn "Starting server"
  backend <- initializeBackend host chdbMasterPort chdbRemoteTable
  startMaster backend initMaster
  -- todo, find slave NodeIds, spawn Server.master.
dispatch ["client", host] = do
  putStrLn "starting chdb client"
  backend <- initializeBackend host chdbClientPort chdbRemoteTable
  clientShell -- host

main :: IO ()
main = do 
  args <- getArgs 
  dispatch args
