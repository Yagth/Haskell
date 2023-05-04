module Pages where

import Control.Applicative
import Datatypes
import Parser

type Line = String

loginPage :: IO ()
loginPage = do
    putStrLn "****Pharma Login*****\n"

    putStr   "Username: "
    username <- getLine
    putStr   "Password: "
    password <- getLine

    inputLines <- readFile "users.txt"
    let users = map (loginUser username password) (words inputLines)
        user  = dropWhile (== Nothing) users
    case user of
        []     -> putStrLn "Incorrect Credentials"
        (x:xs) -> putStrLn $ "Login Successfull\n" ++ show x


adminPage :: IO ()
adminPage = undefined

userPage :: IO ()
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