module Pages where

import Control.Applicative ( Alternative(many) )
import Datatypes ( User, Password, Username )

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p1) = Parser (\input -> do
            (rest, output) <- p1 input
            Just (rest, f output)
        )

instance Applicative Parser where
    pure :: a -> Parser a
    pure = undefined
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser f) (Parser p1) = Parser (\ input -> do
        (rest, output) <- f input
        (rest', output') <- p1 output
        )

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