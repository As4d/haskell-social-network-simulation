module Utils
    ( selectRandom
    , getRandomMessage
    ) where

import System.Random

-- Pick a random element from a list
selectRandom :: [a] -> IO a
selectRandom xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

-- Get a random message content
getRandomMessage :: IO String
getRandomMessage = selectRandom
    [ "Hello"
    , "Hi"
    , "Yo"
    , "Bye"
    , "Cya"
    ]