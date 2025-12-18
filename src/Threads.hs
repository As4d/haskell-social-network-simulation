-- |
-- Module      : Threads
-- Description : User thread behavior for the social network simulation
-- License     : BSD-3-Clause
-- Maintainer  : asadkhan91003@gmail.com
--
-- This module defines the behavior of individual user threads in the concurrent
-- social network simulation. Each user thread operates independently, sending
-- messages to randomly selected recipients at random time intervals.
module Threads (userThread) where

import Control.Concurrent
import System.Random
import Types
import User
import Utils

-- | Defines the behavior of a single user thread in the simulation.
--
-- Each user thread performs the following actions:
--
-- 1. Waits for a random delay between 100ms and 500ms
-- 2. Selects a random recipient from all other users (excluding itself)
-- 3. Sends a message to the selected recipient
--
--
-- ==== __Parameters__
--
-- [@thisUser@] The user represented by this thread
-- [@allUsers@] List of all users in the simulation (including this user)
--
-- ==== __Thread Safety__
--
-- This function is designed to be called concurrently from multiple threads.
-- All message sending operations are thread-safe through the use of MVars
-- in the 'sendMessage' function.
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