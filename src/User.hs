module User (sendMessage, createUser) where

import Control.Concurrent.MVar
import Data.Time.Clock
import Types
import Utils

-- Create a new user with initialised MVars
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

-- Send a message from sender to recipient
sendMessage :: User -> User -> IO ()
sendMessage sender recipient = do
  -- Get random message content
  msgContent <- getRandomMessage

  -- Create the message
  currentTime <- getCurrentTime
  let msg =
        Message
          { sender = username sender,
            recipient = username recipient,
            content = msgContent,
            timestamp = currentTime
          }

  -- Add message to recipient's inbox (thread-safe)
  modifyMVar_ (inbox recipient) $ \msgs -> return (msg : msgs)

  -- Increment recipient's message count (thread-safe)
  modifyMVar_ (messageCount recipient) $ \count -> return (count + 1)

  putStrLn $ username sender ++ " sent message to " ++ username recipient