import Data.ByteString.Lazy as B
import System.Environment

main = do
    (filepath1:filepath2:_) <- getArgs
    copyFile filepath1 filepath2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    
    contents <- B.readFile source
    B.writeFile dest contents