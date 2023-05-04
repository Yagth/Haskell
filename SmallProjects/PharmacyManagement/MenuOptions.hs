module MenuOptions where

import Datatypes
import Distribution.Compat.CharParsing (option)

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
numberOptions = zipWith (\y x-> show y ++ ". " ++ x)  [1..]

displayMenu :: Options -> IO ()
displayMenu options = do
    mapM_ putStrLn (numberOptions options)