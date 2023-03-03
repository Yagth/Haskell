import System.IO
import Data.Char (toUpper)

-- main = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main = do
--     withFile' "girlfriend.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    contents <- f handle
    hClose handle
    return contents

main = do 
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriend.txt" (map toUpper contents)