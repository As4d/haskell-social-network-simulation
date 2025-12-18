-- |
-- Module      : Utils
-- Description : Utility functions for the social network simulation
-- License     : BSD-3-Clause
-- Maintainer  : asadkhan91003@gmail.com
--
-- This module provides general utility functions used throughout the social
-- network simulation, including random selection and message generation.
module Utils
  ( selectRandom,
    getRandomMessage,
  )
where

import System.Random

-- | Selects a random element from a non-empty list.
--
-- This function uses 'randomRIO' to generate a random index and returns
-- the element at that position. It is used for randomly selecting message
-- recipients and message content.
--
--
-- ==== __Examples__
--
-- >>> selectRandom [1, 2, 3, 4, 5]
-- 3  -- (result will vary, any element from the list)
--
-- ==== __Parameters__
--
-- [@xs@] A non-empty list to select from
selectRandom :: [a] -> IO a
selectRandom xs = do
  index <- randomRIO (0, length xs - 1)
  return (xs !! index)

-- | Generates random message content for user communications.
--
-- Returns one of several predefined casual messages at random. This
-- simulates the random nature of social network messages in the simulation.
--
-- The current message pool includes: \"Hello\", \"Hi\", \"Yo\", \"Bye\", \"Cya\"
--
-- ==== __Examples__
--
-- >>> getRandomMessage
-- "Hello"  -- (result will vary, one of the predefined messages)
getRandomMessage :: IO String
getRandomMessage =
  selectRandom
    [ "Hello",
      "Hi",
      "Yo",
      "Bye",
      "Cya"
    ]