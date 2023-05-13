    {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module CommonFuncs where

import Datatypes
import Data.List
import Parser
import System.IO
import Control.Monad (when, unless)
import System.Directory (renameFile)

userFile :: FilePath
userFile = "DB/Users.txt"

medFile :: FilePath
medFile = "DB/Meds.txt"

getMeds :: FilePath -> IO (Maybe [Med])
getMeds filePath = do
    strMeds <- readFile filePath
    let meds = sequence . filter (/= Nothing) $ fmap snd <$> map (runParser parseMed) (lines strMeds)
    return meds

getUsers :: FilePath -> IO (Maybe [User])
getUsers filePath = do
    strUsers <- readFile filePath
    let users = sequence . filter (/= Nothing) $ fmap snd <$> map (runParser parseUser) (lines strUsers)
    return users

findUsers :: Username -> IO (Maybe User)
findUsers username = do
    (Just users) <- getUsers userFile
    let userLookUpTable = map (\user -> (userName user, user)) users
        foundUser       = lookup username userLookUpTable
    return foundUser

findPassword :: Username -> IO (Maybe Password)
findPassword username = do
    strUsers <- readFile userFile

    let result = sequence . dropWhile (Nothing == ) $ map (runParser parsePassword) (lines strUsers)

    case result of
        Nothing -> return Nothing
        Just passwords -> 
            case passwords of
                [] -> return Nothing
                (x:xs) -> return $ Just (snd x)

findPasswords :: [User] -> IO [(User, Password)]
findPasswords users = do
    results <- mapM (findPassword . userName) users
    
    let Just passwords = sequence results
        userPasswords  = zip users passwords
    return userPasswords

findMeds :: Name -> IO (Maybe Med)
findMeds medName = do
    (Just meds) <- getMeds medFile
    let medLookupTable = map (\med -> (name med, med)) meds
        foundMed       = lookup medName medLookupTable

    return foundMed

appendMedToFile :: Med -> IO (Maybe Med)
appendMedToFile med = do
    appendFile medFile (showMed med)
    return (Just med)

appendUserToFile :: User -> IO (Maybe User)
appendUserToFile user = do
    Just password <- findPassword . userName $ user
    appendFile userFile (showUser (user, password))
    return (Just user)

removeMed :: Med -> IO (Maybe Med)
removeMed med = do
    (Just meds) <- getMeds medFile
    let newMeds = delete med meds
        deleted = meds /= newMeds
    if deleted
        then do
            writeFile (medFile ++ ".tmp") (unlines . map showMed $ newMeds)
            renameFile (medFile ++ ".tmp") medFile
            return (Just med)
        else return Nothing

fireUser :: User -> IO (Maybe User)
fireUser user = do
    (Just users) <- getUsers userFile
    let firedUser = CreateUser 
            (firstName user) 
            (lastName user) 
            (userName user) 
            (previlage user) 
            (salary user) 
            NotEmployed
        newUsers = findAndReplace user users
        fired = users/=newUsers
    userPasswords <- findPasswords newUsers
    if fired
        then do
            writeFile (userFile ++ ".tmp") (unlines . map showUser $ userPasswords)
            renameFile (medFile ++ ".tmp") medFile
            return (Just user)
        else return Nothing
    where findAndReplace _ [] = []
          findAndReplace user (x:xs) = 
            if userName x == userName user 
                then user:xs 
                else x:findAndReplace user xs

editMed  :: Name -> Med -> IO (Maybe Med)
editMed medName newMed = do
    (Just meds) <- getMeds medFile
    let (updatedMeds, updated) = replaceMed meds

    if updated
        then do
            writeFile (medFile ++ ".tmp") (unlines . map showMed $ updatedMeds)
            renameFile (medFile ++ ".tmp") medFile
            return (Just newMed)
        else return Nothing

    where replaceMed [] = ([], False)
          replaceMed (x:xs) = if name x == medName 
            then (newMed:xs, True) 
            else let (newS, bool) = replaceMed xs 
                 in (x:newS, bool)

editUser :: Username -> User -> IO (Maybe User)
editUser username newUser = do
    (Just users) <- getUsers userFile

    let (updatedUsers, updated) = replaceUser users
    userPasswords <- findPasswords updatedUsers
    if updated
        then do
            writeFile (userFile ++ ".tmp") (unlines . map showUser $ userPasswords)
            renameFile (userFile ++ ".tmp") userFile
            return (Just newUser)
        else return Nothing

    where replaceUser [] = ([], False)
          replaceUser (x:xs) = if userName x == username
            then (newUser:xs, True)
            else let (newList, bool) = replaceUser xs
                 in (x:newList, bool)

sellMed :: Name -> Int -> IO (Maybe Med)
sellMed medName amount' = do
    med <- findMeds medName
    let result = case med of
                  Just med'  ->Just $ CreateMed (name med') (amount med' - amount') (price med')
                  _          -> Nothing
    case result of
     Just updatedMed -> editMed medName updatedMed
     _               -> return Nothing

addMed :: [String] ->  IO (Maybe Med)
addMed inputs = do
    let result = runParser parseMed (unwords inputs)

    case result of 
         Just (_, med) -> do
            appendMedToFile med
         _             -> return Nothing

addUser :: [String] -> IO (Maybe User)
addUser inputs = do
    let result = runParser parseUser (unwords inputs)

    case result of
        Just (_, user) -> do
            appendUserToFile user
        _              -> return Nothing

showMed :: Med -> String
showMed med = unwords [f med | f <- [name, show . amount, show . price]]

showUser :: (User, Password) -> String
showUser (user, password) = unwords [f user | f <- [userName, const password, firstName, lastName ,show . previlage, show . salary, show . status]]
    
createUserName :: String -> String -> IO String
createUserName firstname lastname =do
    let [len1, len2] = map ((`div` 2) . length) [firstname, lastname]
        username = take len1 firstname ++ take len2 lastname
    
    uniqueUsername username 0

    where uniqueUsername username count = do
            user <- findUsers username
            case user of
                Nothing -> return username
                _       -> uniqueUsername (username ++ show count) (count + 1)

fmapT :: (a -> a) -> (a, b) -> (a, b)
fmapT f (x,y) = (f x, y)

readMaybe ::(Read a) => String -> Maybe a
readMaybe input = let xs = reads input 
    in case xs of
        [] -> Nothing
        [(value, rest)] -> Just value