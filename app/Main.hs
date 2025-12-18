module Main (main) where

import Types
import Control.Concurrent
import Control.Monad (forM)

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

-- This is what each user thread will do
userThread :: User -> IO ()
userThread user = do
    putStrLn $ username user ++ " thread started!"
    -- TODO: Later this will send random messages
    threadDelay 2000000  -- Wait 2 seconds
    putStrLn $ username user ++ " thread finished!"

main :: IO ()
main = do
    putStrLn "Creating 10 users..."
    
    -- Step 1: Create 10 users
    users <- forM [1..10] createUser
    putStrLn $ "Created " ++ show (length users) ++ " users\n"
    
    -- Step 2: Spawn 10 threads (one for each user)
    putStrLn "Spawning threads..."
    threadIds <- forM users $ \user -> do
        forkIO (userThread user)
    
    putStrLn $ "Spawned " ++ show (length threadIds) ++ " threads\n"
    
    -- Step 3: Wait for threads to finish
    threadDelay 3000000  -- Wait 3 seconds
    
    putStrLn "\nAll threads completed!"