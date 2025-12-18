module Main (main) where

import Control.Concurrent
import Control.Monad (forM)
import Threads
import Types
import User

main :: IO ()
main = do
  putStrLn "Creating 10 users..."
  users <- forM [1 .. 10] createUser

  putStrLn "Spawning threads...\n"
  threadIds <- forM users $ \user -> do
    forkIO (userThread user users)

  -- Wait a bit to see some messages
  threadDelay 2000000

  putStrLn "\nDone for now!"