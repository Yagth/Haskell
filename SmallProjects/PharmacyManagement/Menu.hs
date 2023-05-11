module Menu where

import Datatypes
import Distribution.Compat.CharParsing (option)
import Parser (parseMed, runParser, parseUser)
import CommonIOFuncs
import CommonFuncs
import Data.Char (toUpper)

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
    case choice of
        "" -> return ()
        _  -> editOrDeleteMed choice meds


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

editMedForm :: String -> [Med] -> IO ()
editMedForm choice meds= do
    clearScreen
    putStrLn "****Edit Medicine****\n"
    case readMaybe choice of
        Nothing -> do
            if choice == ""
                then return ()
                else do
                 putStrLn "\nInvalid choice!!\n"
                 systemPause
                 displayMeds
        Just medNo -> do
            medHeader
            putStr $ show medNo ++ ".  "
            print $ meds !! (medNo - 1)
            putStrLn ""

            putStr "Name: "
            medName <- getLine
            putStr "Amount: "
            medAmount <- getLine
            putStr "Price: "
            medPrice <- getLine

            let newName   = newMedInfo medName (meds !! (medNo - 1)) name
                newAmount = newMedInfo medAmount (meds !! (medNo - 1)) (show . amount)
                newPrice  = newMedInfo medPrice (meds !! (medNo - 1)) (show . price)
                newMed    = snd <$> runParser parseMed (unwords [newName, newAmount, newPrice])
            case newMed of
                Nothing -> do
                    putStrLn "Couldn't create med due to some error\n"
                    systemPause
                    displayMeds                
                Just med -> do
                    editMed (name (meds !! (medNo - 1))) med
                    putStrLn "\nUpdated information successfully\n"
                    systemPause
                    displayMeds
                
    
    where newMedInfo newInfo med f | newInfo == "" =  f med | otherwise = newInfo

medDetailOptions :: [String]
medDetailOptions = ["Edited Selected Medicine", "Delete Selected Medicine"]

editOrDeleteMed :: String -> [Med] -> IO ()
editOrDeleteMed choice meds = do
    clearScreen
    putStrLn "****Med Detail****\n"
    mapM_ putStrLn (numberOptions medDetailOptions)
    
    putStr "\nChoice: "
    choosenOption <- getLine
    case choosenOption of
        "1" -> editMedForm choice meds
        "2" -> deleteMedForm choice meds
        _   -> do
            putStrLn "No such choice\n"
            putStr "Retry? (N/Y): "
            retry <- getLine
            case map toUpper retry of
                "Y" -> editOrDeleteMed choice meds
                _   -> displayMeds

deleteMedForm :: String -> [Med] -> IO ()
deleteMedForm choice meds = do
    case readMaybe choice of
        Nothing -> do
            if choice == ""
                then return ()
                else do
                 putStrLn "\nInvalid choice!!\n"
                 systemPause
                 displayMeds
        Just medNo -> do
            putStrLn ""
            putStr $ "Are you sure you want to delete med number " ++ show medNo ++ " (N/Y): "
            confirm <- getLine
            if  map toUpper confirm == "Y"
                then do 
                    med <- removeMed (meds !! (medNo - 1))
                    case med of
                        Nothing -> do 
                            putStrLn ""
                            putStrLn "Couldn't delete med due to some error\n"
                        Just _ -> do
                            putStrLn ""
                            putStrLn "Deleted med successfully\n"
                    systemPause
                else do return ()
    displayMeds


wrongChoice :: (Maybe User -> IO ()) -> Maybe User -> IO ()
wrongChoice callBack user = do
    putStrLn "\nNo Such Choice"
    systemPause
    callBack user