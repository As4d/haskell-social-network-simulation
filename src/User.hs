-- |
-- Module      : User
-- Description : User management and message sending operations
-- License     : BSD-3-Clause
-- Maintainer  : asadkhan91003@gmail.com
--
-- This module provides functionality for creating users and sending messages
-- between them in a thread-safe manner. It handles the initialisation of
-- thread-safe data structures (MVars) and ensures concurrent message delivery
-- without race conditions.
module User (sendMessage, createUser) where

import Control.Concurrent.MVar
import Data.Time.Clock
import Types
import Utils

-- | Creates a new user with initialised thread-safe storage.
--
-- This function initialises a user with an empty inbox and a message counter
-- set to zero. Both the inbox and counter use 'MVar' to ensure thread-safe
-- access across concurrent user threads.
--
-- The username is automatically generated as \"User\" followed by the user ID.
--
-- ==== __Examples__
--
-- >>> user <- createUser 1
-- >>> username user
-- "User1"
--
-- >>> userId user
-- 1
createUser :: Int -> IO User
createUser uid = do
  emptyInbox <- newMVar []
  counter <- newMVar 0
  return
    User
      { userId = uid,
        username = "User" ++ show uid,
        inbox = emptyInbox,
        messageCount = counter
      }

-- | Sends a message from one user to another in a thread-safe manner.
--
-- This function performs the following operations atomically:
--
-- 1. Generates random message content
-- 2. Creates a timestamped message
-- 3. Adds the message to the recipient's inbox (thread-safe)
-- 4. Increments the recipient's message count (thread-safe)
-- 5. Logs the message sending event to stdout
--
-- The use of 'modifyMVar_' ensures that both inbox and counter updates
-- are atomic and thread-safe, preventing race conditions when multiple
-- threads send messages simultaneously.
--
-- ==== __Parameters__
--
-- [@fromUser@] The user sending the message
-- [@toUser@] The user receiving the message
sendMessage :: User -> User -> MVar [Message] -> IO ()
sendMessage fromUser toUser messageLog = do
  -- Get random message content
  msgContent <- getRandomMessage

  -- Create the message
  currentTime <- getCurrentTime
  let msg =
        Message
          { sender = username fromUser,
            recipient = username toUser,
            content = msgContent,
            timestamp = currentTime
          }

  -- Add message to recipient's inbox (thread-safe)
  modifyMVar_ (inbox toUser) $ \msgs -> return (msg : msgs)

  -- Increment recipient's message count (thread-safe)
  modifyMVar_ (messageCount toUser) $ \count -> return (count + 1)

  -- Log the message (thread-safe)
  modifyMVar_ messageLog $ \logs -> return (msg : logs)

  putStrLn $ username fromUser ++ " sent message to " ++ username toUser