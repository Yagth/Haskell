module Menu where

import Datatypes
import Distribution.Compat.CharParsing (option)
import Parser (parseMed, runParser, parseUser)
import CommonIOFuncs
import CommonFuncs

type Options = [String]


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

displayMenu :: Options -> IO ()
displayMenu options = do
    mapM_ putStrLn (numberOptions options)

displayMeds :: IO ()
displayMeds = do
    clearScreen
    Just meds <- getMeds medFile
    putStrLn "****List Of all Meds****\n"
    putStrLn "No. Name\tAmount\tPrice\n"
    mapM_ putStrLn (numberOptions . map (\line -> " " ++ show line) $ meds)
    putStrLn ""
    systemPause

displayUsers :: IO ()
displayUsers = do
    clearScreen
    Just users <- getUsers userFile
    putStrLn "****List of all Users****\n"
    putStrLn "No. F-name\tL-name\tPrev\tSalary\tStatus\n"
    mapM_ putStrLn (numberOptions . map show $ users)
    putStrLn ""
    systemPause

wrongChoice :: (Maybe User -> IO ()) -> Maybe User -> IO ()
wrongChoice callBack user = do
    putStrLn "\nNo Such Choice"
    systemPause
    callBack user

sellMedForm :: IO ()
sellMedForm = do
    clearScreen
    putStrLn "****Sell Meds****\n"
    putStr   "MedName: "
    medName <- getLine
    putStr   "Amount: "
    amount  <- getLine

    putStrLn ""