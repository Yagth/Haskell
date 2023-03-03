import System.Environment
import System.IO
import Data.List
import System.Directory (removeFile, renameFile)

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch

    action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("add", add),
    ("view", view),
    ("remove", remove),
    ("bump", bump)
    ]

add :: [String] -> IO ()
add [filename, task] = appendFile filename (task ++ "\n")

view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename 
    let todoTasks = lines contents
        numberedTasks = unlines $ zipWith (\n line -> show n ++ "-" ++ line) [0..] todoTasks
    putStrLn $ "To Do List in " ++ filename
    putStrLn ""
    putStrLn numberedTasks

remove :: [String] -> IO ()
remove [filename, numberString] = do
    handle <- openFile filename ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    
    let todoTasks = lines contents
        index = read numberString
        newTasks = delete (todoTasks !! index) todoTasks

    hPutStr tempHandle (unlines newTasks)

    hClose tempHandle
    hClose handle

    removeFile filename
    renameFile tempName filename

bump :: [String] -> IO ()
bump [filename, numberString] = do
    handle <- openFile filename ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"

    contents <- hGetContents handle

    let todoTasks = lines contents
        index = read numberString
        task = todoTasks !! index
        tempTodoList = delete task todoTasks
        newTodoList = task : tempTodoList

    hPutStr tempHandle (unlines newTodoList)
    hClose handle
    hClose tempHandle

    removeFile filename
    renameFile tempName filename
