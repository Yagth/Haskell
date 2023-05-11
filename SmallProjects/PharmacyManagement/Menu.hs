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

medHeader :: IO ()
medHeader = putStrLn "No. Name\tAmount\tPrice\n"


displayMeds :: IO ()
displayMeds = do
    clearScreen
    Just meds <- getMeds medFile
    putStrLn "****List Of all Meds****\n"
    medHeader
    mapM_ putStrLn (numberOptions . map (\line -> " " ++ show line) $ meds)
    putStrLn ""
    putStr "Choice: "
    choice <- getLine
    case readMaybe choice of
        Nothing -> return ()
        Just medNo -> do
            putStrLn ""
            medHeader
            putStr $ show medNo ++ ".  "
            print $ meds !! (medNo - 1)
            putStrLn ""
            
            systemPause

            return ()



displayUsers :: IO ()
displayUsers = do
    clearScreen
    Just users <- getUsers userFile
    putStrLn "****List of all Users****\n"
    mapM_ putStrLn (numberOptions . map show $ users)
    putStrLn ""
    systemPause

addMedForm :: IO ()
addMedForm = do
    clearScreen
    putStrLn "****Add new Medicine****\n"
    putStr "Med Name: "
    medName <- getLine
    putStr "Amount: "
    medAmount <- getLine
    putStr "Price: "
    medPrice <- getLine
    newMed <- addMed [medName, medAmount, medPrice]

    case newMed of
        Just med -> do
            putStrLn "\nNew med with the following info created!!\n"
            medHeader
            putStrLn ("1. " ++ show med)
        Nothing    -> do
            putStrLn "Couldn't create Med due to some error\n"

    systemPause
    displayMeds

sellMedForm :: IO ()
sellMedForm = do
    clearScreen
    putStrLn "****Sell Medicine****\n"
    putStr   "Med Name: "
    medName <- getLine
    putStr   "amount: "
    medAmount  <- getLine
    med <- sellMed medName (read medAmount)            
    case med of
        Just med' -> do
            putStrLn ""
            putStrLn "****Updated information****\n"
            putStrLn $ "Sold med " ++name med' ++ " with amount: " ++ medAmount ++ "\n"
            medHeader
            print med'
        Nothing  -> do
            putStrLn "Couldn't perform med update due to some error\n"
            putStr "Retry?N/:  "
            choice <- getLine
            if choice == "Y"
                then sellMedForm
                else displayMeds
    

wrongChoice :: (Maybe User -> IO ()) -> Maybe User -> IO ()
wrongChoice callBack user = do
    putStrLn "\nNo Such Choice"
    systemPause
    callBack user