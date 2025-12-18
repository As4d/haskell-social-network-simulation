module Types where

import Control.Concurrent.MVar
import Data.Time.Clock

data User = User 
    { userId       :: Int
    , username     :: String
    , inbox        :: MVar [Message]
    , messageCount :: MVar Int
    }

data Message = Message 
    { sender    :: String
    , recipient :: String
    , content   :: String
    , timestamp :: UTCTime
    } deriving (Show)