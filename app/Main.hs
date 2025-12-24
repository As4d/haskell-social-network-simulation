module Main (main) where

import Control.Concurrent
import Control.Monad (forM, forM_, when)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Format
import Threads
import Types
import User

main :: IO ()
main = do
  putStrLn "Creating 10 users..."
  users <- forM [0 .. 9] createUser

  -- Create counter starting at 100
  messagesRemaining <- newMVar 100

  -- Create message log
  messageLog <- newMVar []

  putStrLn "Spawning threads...\n"
  forM_ users $ \user -> do
    forkIO (userThread user users messagesRemaining messageLog)

  -- Wait until counter reaches 0
  waitUntilZero messagesRemaining

  -- Give threads time to finish
  threadDelay 500000

  -- Display results
  forM_ users $ \user -> do
    count <- readMVar (messageCount user)
    putStrLn $ username user ++ " received " ++ show count ++ " messages"

  -- Show total
  total <- sum <$> mapM (readMVar . messageCount) users
  putStrLn $ "\nTotal: " ++ show total ++ " messages"

  -- Export message log
  putStrLn "\nExporting message log..."
  exportMessageLog messageLog

waitUntilZero :: MVar Int -> IO ()
waitUntilZero counter = do
  remaining <- readMVar counter
  when (remaining > 0) $ do
    threadDelay 100000
    waitUntilZero counter

exportMessageLog :: MVar [Message] -> IO ()
exportMessageLog logMVar = do
  messages <- readMVar logMVar

  -- Sort by timestamp (oldest first)
  let sortedMessages = sortBy (comparing timestamp) messages

  writeFile "message_log.txt" $ unlines $ map formatMessage sortedMessages
  putStrLn "Message log saved to message_log.txt"

formatMessage :: Message -> String
formatMessage msg =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (timestamp msg)
    ++ " | "
    ++ sender msg
    ++ " -> "
    ++ recipient msg
    ++ " | "
    ++ content msg