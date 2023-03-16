import System.Environment
import System.IO
import Data.List
import System.Directory (removeFile, renameFile)

main = do
    (command:args) <- getArgs
    let result = lookup command dispatch
        action = case result of
            Nothing -> help
            (Just function) -> function

    action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [
    ("--add", add),
    ("--view", view),
    ("--remove", remove),
    ("--bump", bump),
    ("-a", add),
    ("-v", view),
    ("-r", remove),
    ("-b", bump),
    ("--help", help)
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
 
help ::[String] -> IO ()
help xs = do
    let helpStr = "Help menu :\n"++
                  "--add | -a <file name> <Todo list item in double coutes> : To add new Item to the file\n"++
                  "--view | -v <file name> : To view todolist\n" ++
                  "--remove | -r <file name> <Todo item number> : To remove a todo list item\n" ++
                  "--bump | -b <file name> <Todo item number> : To move a todo item to top of the list\n"++
                  "--help | -h : To see the list of commands\n"
    putStrLn helpStr