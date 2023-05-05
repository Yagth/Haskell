module Datatypes where
import Data.List (intercalate)


type Name      = String
type Username  = String
type Password  = String
type Salary    = Float

data Pharmacy  = CreatePharm {users :: [User], meds :: [Med]}

data User      = CreateUser  {
        firstName  :: Name,
        lastName   :: Name,
        userName   :: Username, 
        previlage  :: Previlage,
        salary     :: Salary, 
        status     :: Status
    } deriving Eq

instance Show User where
    show :: User -> String
    show user = intercalate "," [firstName user, lastName user, (show . previlage) user, (show . salary) user, (show . status) user]

data Med      = CreateMed   {name :: Name, amount :: Int, price :: Float} deriving (Eq)

instance Show Med where
    show :: Med -> String
    show med = intercalate "," [name med, (show . amount) med, (show . price) med]

data Status    = Onshift | OffShift | OnVacation | NotEmployed deriving (Show, Eq)

data Previlage = Admin   | Normal deriving (Show, Eq)


-- Pharmacy will have, login page, admin page, user page.
-- Loginpage, asks for username and password and checks from the file (password save using hash)
-- Admin page, CRUD operation on meds, CRUD operations on Users, logout
-- User page, See lists of meds
-- All users, Sell medicine, Add medicine