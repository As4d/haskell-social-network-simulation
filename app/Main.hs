module Main (main) where

import Control.Concurrent
import Control.Monad (forM, forM_)
import Threads
import Types
import User

main :: IO ()
main = do
  putStrLn "Creating 10 users..."
  users <- forM [1 .. 10] createUser

  -- Create counter starting at 100
  messagesRemaining <- newMVar 100

  putStrLn "Spawning threads...\n"
  threadIds <- forM users $ \user -> do
    forkIO (userThread user users messagesRemaining)

  -- Wait until counter reaches 0
  waitUntilZero messagesRemaining

  -- Display results
  forM_ users $ \user -> do
    count <- readMVar (messageCount user)
    putStrLn $ username user ++ " received " ++ show count ++ " messages"

waitUntilZero :: MVar Int -> IO ()
waitUntilZero counter = do
  remaining <- readMVar counter
  if remaining > 0
    then do
      threadDelay 100000
      waitUntilZero counter
    else return ()