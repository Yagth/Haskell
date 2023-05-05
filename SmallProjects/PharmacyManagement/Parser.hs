module Parser where

import Control.Applicative ( Alternative((<|>), empty, many) )
import Datatypes


newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p1) = Parser (\input -> do
            (rest, output) <- p1 input
            Just (rest, f output)
        )

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\input -> Just (input, x))
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser p1) (Parser p2) = Parser (\ input -> do
        (rest, f) <- p1 input
        (rest', output) <- p2 rest
        Just (rest', f output)
        )

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) (Parser p1) (Parser p2) = Parser (\input-> p1 input <|> p2 input)


charP :: Char -> Parser Char
charP x = Parser f
    where f [] = Nothing
          f (y:ys)
             | x == y = Just (ys, x)
             | otherwise = Nothing

parseWord :: Parser String
parseWord = Parser $  
    \input ->
    let (token, rest) = span (/= ',') input
    in Just (rest, token)

parseFields :: Parser [String]
parseFields = sepBy (charP ',') parseWord

parseUser :: Parser User
parseUser = Parser $ \input -> do
    (rest, output) <- runParser parseFields input
    user <- createUser output
    Just (rest, user)
    
    where 
    createUser [un, _ , fn, ln, plg, sl, st] = Just CreateUser{
        firstName = fn,
        lastName  = ln,
        userName  = un,
        previlage = parsePrevilage plg,
        salary    = read sl,
        status    = parseStatus st
        }
    createUser xs                            = Nothing
    parseStatus strStat
        | strStat == "Onshift"    = Onshift 
        | strStat == "OffShift"   = OffShift
        | strStat == "OnVacation" = OnVacation
        | otherwise               = NotEmployed

    parsePrevilage strPrev 
        | strPrev == "Admin" = Admin 
        | otherwise = Normal

parseMed :: Parser Med
parseMed = Parser $ \input -> do
    (rest, output) <- runParser parseFields input
    med            <- createMed output
    return (rest, med)
    where createMed [name , amount , price] = Just $ CreateMed name (read amount) (read price)
          createMed xs                      = Nothing
          

parsePassword :: Parser Password
parsePassword = getPassword <$> parseFields
    where getPassword (_:psw:xs) = psw
          getPassword xs         = ""

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []