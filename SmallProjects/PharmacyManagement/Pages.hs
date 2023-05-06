module Pages where

import Control.Applicative
import Datatypes
import Parser
import Menu
import CommonIOFuncs
import CommonFuncs

type Line = String

loginPage :: IO ()
loginPage = do
    clearScreen
    putStrLn "****Pharma Login*****\n"

    putStr   "Username: "
    username <- getLine
    putStr   "Password: "
    password <- getLine

    inputLines <- readFile userFile

    let users = map (loginUser username password) (lines inputLines)
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

logout :: IO ()
logout = do 
    clearScreen
    putStrLn "Logout Successfull"
    systemPause
    loginPage

adminFuncs :: [(String, IO ())]
adminFuncs = [
    ("1", sellMedForm),
    ("2", addMedForm),
    ("3", displayMeds),
    ("4", displayUsers),
    ("5", logout)
    ]

adminPage :: Maybe User -> IO ()
adminPage user'@(Just user) = do
    clearScreen
    putStrLn $ "Welcome: Admin - " ++ userName user ++ "\n"
    displayMenu (commonOptions ++ adminOptions)

    putStr "\nChoice: "
    option <- getLine
    let result = lookup option adminFuncs
        action = case result of
            Just action -> action
            Nothing     -> wrongChoice adminPage user'
    
    action
    adminPage user'
    
    return ()
adminPage Nothing     = do
    putStrLn "Unauthorized"
    systemPause
    loginPage

userPage :: Maybe User -> IO ()
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