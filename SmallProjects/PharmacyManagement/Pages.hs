module Pages where

import Control.Applicative
import Datatypes
import Parser
import MenuOptions

import System.Process

type Line = String

loginPage :: IO ()
loginPage = do
    clearScreen
    putStrLn "****Pharma Login*****\n"

    putStr   "Username: "
    username <- getLine
    putStr   "Password: "
    password <- getLine

    inputLines <- readFile "users.txt"
    let users = map (loginUser username password) (words inputLines)
        user  = dropWhile (== Nothing) users
    case user of
        []     -> do 
            putStrLn "Incorrect Credentials"
            systemPause
            loginPage
        (x:xs) -> do 
            putStrLn "Login Successfull\n"
            systemPause
            adminPage x


adminPage :: Maybe User -> IO ()
adminPage (Just user) = do
    clearScreen
    putStrLn $ "Welcome: Admin - " ++ userName user ++ "\n"
    displayMenu (commonOptions ++ adminOptions)

    putStr "\nChoice: "
    option <- getLine

    return ()
adminPage Nothing     = do
    putStrLn "Unauthorized"
    systemPause
    loginPage

    

userPage :: User -> IO ()
userPage = undefined

loginUser :: Username -> Password -> Line -> Maybe User
loginUser uname pass line = 
    if pass /= pass2 || userName user /= uname
        then Nothing
        else do
            Just user
    where Just (_ , pass2) = runParser parsePassword line <|> Just ("","")
          Just (_ ,  user) = runParser parseUser line     <|> Just ("", nullUser)
          nullUser = CreateUser "" "" "" Normal 0.0 NotEmployed

clearScreen :: IO ()
clearScreen = callCommand "clear"

systemPause :: IO ()
systemPause = do 
    putStrLn "Press any Enter key to continue..."
    _ <- getLine
    return ()