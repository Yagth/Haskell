import System.IO
import System.IO.Error
import Data.Char (toUpper)
import System.Random
import System.Environment
import Control.Exception (catch)

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
    toTry `catch` handler

--The module System.Random Not found

toTry :: IO()
toTry = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriend.txt" (map toUpper contents)

handler :: IOError -> IO ()
handler e 
    | isAlreadyInUseError e = putStrLn "The file is already in use. Sorry!!"
    | isDoesNotExistError e = case ioeGetFileName e of 
        Just path -> putStrLn $ "File doesn't exist at "++ path ++ " location"
        Nothing -> putStrLn "File doesn't exist at unknown location"
    | otherwise = ioError e

randoms' ::(RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen