{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module CommonFuncs where

import Datatypes
import Data.List
import Parser
import System.IO
import Control.Monad (when, unless)
import GHC.IO.Device (IODevice(close))
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

findMeds :: Name -> IO (Maybe Med)
findMeds medName = do
    (Just meds) <- getMeds medFile
    let medLookupTable = map (\med -> (name med, med)) meds
        foundMed       = lookup medName medLookupTable

    return foundMed

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

showMed :: Med -> String
showMed med = unwords [f med | f <- [name, show . amount, show . price]]

fmapT :: (a -> a) -> (a, b) -> (a, b)
fmapT f (x,y) = (f x, y)