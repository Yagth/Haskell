module CommonIOFuncs where

import System.Process

clearScreen :: IO ()
clearScreen = callCommand "clear"

systemPause :: IO ()
systemPause = do 
    putStrLn "Press Enter key to continue..."
    _ <- getLine
    return ()