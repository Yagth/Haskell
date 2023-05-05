module CommonFuncs where

import Datatypes
import Data.List
import Parser
import System.IO
import Control.Monad (when, unless)

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

findMeds :: Name -> IO (Maybe Med)
findMeds medName = do
    (Just meds) <- getMeds medFile
    let medLookupTable = map (\med -> (name med, med)) meds
        foundMed       = lookup medName medLookupTable

    return foundMed

editMed  :: Name -> Med -> IO (Maybe Med)
editMed medName newMed = do
    handle <- openFile medFile ReadWriteMode

    let loop :: IO (Maybe Med)
        loop = do
            eof <- hIsEOF handle
            if eof
                then return Nothing
                else do
                    line <- hGetLine handle
                    let med = find (== medName ) (words line)
                        newLine = case med of
                         Just name -> show newMed
                         _         -> line
                        found = newLine /= line
                    if found
                        then return (Just newMed)
                        else loop  
    loop

    