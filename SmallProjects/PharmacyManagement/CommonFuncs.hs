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

showMed :: Med -> String
showMed med = unwords [f med | f <- [name, show . amount, show . price]]


fmapT :: (a -> a) -> (a, b) -> (a, b)
fmapT f (x,y) = (f x, y)

readMaybe ::(Read a) => String -> Maybe a
readMaybe input = let xs = reads input 
    in case xs of
        [] -> Nothing
        [(value, rest)] -> Just value