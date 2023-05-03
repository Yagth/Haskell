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
    putStrLn $ username ++ password

adminPage :: IO ()
adminPage = undefined

userPage :: IO ()
userPage = undefined

-- findUserInFile :: Username -> IO (Maybe User)
-- findUserInFile uname pass = undefined

loginUser :: Username -> Password -> Line -> Maybe User
loginUser uname pass line = 
    if pass /= pass2 || userName user /= uname
        then Nothing
        else do
            Just user
    where Just (_ , pass2) = runParser parsePassword line <|> Just ("","")
          Just (_ ,  user) = runParser parseUser line     <|> Just ("", nullUser)
          nullUser = CreateUser "" "" "" Normal 0.0 NotEmployed