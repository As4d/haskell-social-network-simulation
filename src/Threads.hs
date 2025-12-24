-- |
-- Module      : Threads
-- Description : User thread behavior for the social network simulation
-- License     : BSD-3-Clause
-- Maintainer  : asadkhan91003@gmail.com
--
-- This module defines the behavior of individual user threads in the concurrent
-- social network simulation. Each user thread operates independently, sending
-- messages to randomly selected recipients at random time intervals until all
-- available message slots are claimed.
module Threads (userThread) where

import Control.Concurrent
import Control.Monad (when)
import System.Random
import Types
import User
import Utils

-- | Defines the behavior of a single user thread in the simulation.
--
-- Each user thread performs the following actions in a loop:
--
-- 1. Atomically claims a message slot from the shared counter
-- 2. If a slot was successfully claimed, waits for a random delay (100-500ms)
-- 3. Selects a random recipient from all other users (excluding itself)
-- 4. Sends a message to the selected recipient
-- 5. Repeats until no message slots remain
--
-- ==== __Parameters__
--
-- [@thisUser@] The user represented by this thread
-- [@allUsers@] List of all users in the simulation (including this user)
-- [@messagesRemaining@] Shared MVar counter tracking remaining message slots to claim
--
-- ==== __Thread Safety__
--
-- This function uses atomic operations via 'modifyMVar' to prevent race conditions
-- when claiming message slots. The check-and-decrement operation is performed
-- atomically, ensuring exactly the target number of messages are sent across all
-- threads. Message sending operations are also thread-safe through the use of MVars
-- in the 'sendMessage' function.
userThread :: User -> [User] -> MVar Int -> MVar [Message] -> IO ()
userThread thisUser allUsers messagesRemaining messageLog = loop
  where
    loop = do
      -- Atomically claim a message slot
      canSend <- modifyMVar messagesRemaining $ \remaining -> do
        if remaining > 0
          then return (remaining - 1, True) -- Claim slot
          else return (remaining, False) -- No slots left
      when canSend $ do
        -- Random delay between 100ms and 500ms
        delay <- randomRIO (100000, 500000)
        threadDelay delay

        -- Select random recipient (excluding self)
        let otherUsers = filter (\u -> userId u /= userId thisUser) allUsers
        recipient <- selectRandom otherUsers

        -- Send message
        sendMessage thisUser recipient messageLog

        -- Loop to claim another slot
        loop