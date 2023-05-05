module Menu where

import Datatypes
import Distribution.Compat.CharParsing (option)
import Parser (parseMed, runParser, parseUser)

type Options = [String]

userFile :: FilePath
userFile = "DB/Users.txt"

medFile :: FilePath
medFile = "DB/Meds.txt"


adminOptions :: Options
adminOptions = ["List All Medicines","List All Users","Logout"]

allMedsOptions :: Options
allMedsOptions = ["List Meds", "Remove Meds", "Edit Meds"]

allUsersOptions :: Options
allUsersOptions = ["List Users", "Add User", "Remove User", "In shift Users"]

userOptions :: Options
userOptions = [""]

commonOptions :: Options
commonOptions = ["Sell Meds", "Add meds"]

numberOptions :: Options -> Options
numberOptions = zipWith (\y x-> show y ++ ". " ++ x) [1..]

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

displayMenu :: Options -> IO ()
displayMenu options = do
    mapM_ putStrLn (numberOptions options)

displayMeds :: IO ()
displayMeds = do
    Just meds <- getMeds medFile
    mapM_ putStrLn (numberOptions . map show $ meds)

displayUsers :: IO ()
displayUsers = do
    Just users <- getUsers userFile
    mapM_ putStrLn (numberOptions . map show $ users)