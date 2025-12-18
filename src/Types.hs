{-|
Module      : Types
Description : Core data types for the social network simulation
License     : BSD-3-Clause
Maintainer  : asadkhan91003@gmail.com

This module defines the fundamental data types used in the concurrent social
network simulation. It provides thread-safe user representations and immutable
message structures for communication between user threads.
-}

module Types(User(..), Message(..)) where

import Control.Concurrent.MVar
import Data.Time.Clock

-- | Represents a user in the social network simulation.
--
-- Each user operates in their own thread and maintains a thread-safe inbox
-- for receiving messages from other users. The 'MVar' types ensure safe
-- concurrent access to the inbox and message count across multiple threads.
data User = User 
    { userId       :: Int              -- ^ Unique identifier for the user (1-10)
    , username     :: String           -- ^ Display name of the user
    , inbox        :: MVar [Message]   -- ^ Thread-safe inbox for receiving messages
    , messageCount :: MVar Int         -- ^ Thread-safe counter for total messages received
    }

-- | Represents a message sent between users in the social network.
--
-- Messages are immutable once created and capture all relevant information
-- about a communication event, including sender, recipient, content, and
-- the exact time the message was created.
data Message = Message 
    { sender    :: String   -- ^ Username of the message sender
    , recipient :: String   -- ^ Username of the message recipient
    , content   :: String   -- ^ The actual message content
    , timestamp :: UTCTime  -- ^ UTC timestamp when the message was created
    } deriving (Show)