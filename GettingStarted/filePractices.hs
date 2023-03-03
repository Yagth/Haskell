import GHC.IO.Handle.FD
import System.Directory.Internal.Prelude
import Data.Text.IO
import Control.Exception (handle)

-- main = do
--     handle <- openFile "girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     Data.Text.IO.putStr contents
--     hClose handle

main = do
    withFile' "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        Data.Text.IO.putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    contents <- f handle
    hClose handle
    return contents