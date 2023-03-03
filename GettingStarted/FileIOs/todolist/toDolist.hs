import System.IO
import System.Directory
import Data.List
import GHC.Data.ShortText (ShortText(contents))

main = do
    handle <- openFile "todolist.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let tasksList = lines contents
        numberedTasks = zipWith (\n line -> show n ++ "-" ++ line) [0..] tasksList
    
    putStrLn "Your Todo List"
    putStrLn $ unlines numberedTasks
    putStrLn "Which number do you want to delete?"
    numberString <- getLine

    let number = read numberString
        newTasksList = delete (tasksList !! number) tasksList
    
    hPutStr tempHandle $ unlines newTasksList
    hClose handle
    hClose tempHandle

    removeFile "todolist.txt"
    renameFile tempName "todolist.txt"
