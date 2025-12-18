module Main (main) where

import Types
import Control.Concurrent.MVar

createUser :: Int -> IO User
createUser uid = do
    emptyInbox <- newMVar []      
    counter <- newMVar 0          
    return User
        { userId = uid
        , username = "User" ++ show uid
        , inbox = emptyInbox
        , messageCount = counter
        }

main :: IO ()
main = do
    putStrLn "Creating User Test"
    
    user1 <- createUser 1

    putStrLn $ "Example: " ++ username user1