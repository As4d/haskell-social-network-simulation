module Threads (userThread) where

import Control.Concurrent
import System.Random
import Types
import User
import Utils

-- What each user thread does
userThread :: User -> [User] -> IO ()
userThread thisUser allUsers = do
  -- Random delay between 100ms and 500ms
  delay <- randomRIO (100000, 500000)
  threadDelay delay

  -- Select random recipient (excluding self)
  let otherUsers = filter (\u -> userId u /= userId thisUser) allUsers
  recipient <- selectRandom otherUsers

  -- Send message
  sendMessage thisUser recipient

-- TODO: Loop until 100 messages