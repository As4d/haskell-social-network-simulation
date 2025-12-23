module Main (main) where

import Control.Concurrent
import Control.Monad (forM)
import Control.Monad (forM_)
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

  -- Main thread just waits
  waitUntilDone users 100
  
  -- Display results
  forM_ users $ \user -> do
      count <- readMVar (messageCount user)
      putStrLn $ username user ++ " received " ++ show count ++ " messages"

waitUntilDone :: [User] -> Int -> IO ()
waitUntilDone users target = do
    counts <- mapM (readMVar . messageCount) users
    let total = sum counts
    if total < target
        then do
            threadDelay 100000
            waitUntilDone users target
        else return ()