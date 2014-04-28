module Main where

import System.Environment

dispatch :: String -> IO ()
dispatch "server" = do
  putStrLn "Starting server"
  -- Run a server.
dispatch "" = do
  -- Run a client.

main :: IO ()
main = do
  [arg] <- getArgs
  dispatch arg
