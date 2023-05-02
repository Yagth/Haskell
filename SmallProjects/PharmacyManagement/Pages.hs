module Pages where

import Control.Applicative ( Alternative(many) )
import Datatypes ( User, Password, Username )

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

findUserInFile :: Username -> Password -> IO (Maybe User)
findUserInFile uname pass = undefined

parse :: Char -> String -> [String]
parse sep line = many (separate line)
    where separate = takeWhile (/= sep)