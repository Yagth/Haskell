module Datatypes where


type Name      = String
type Username  = String
type Password  = String
type Salary    = Float

data Pharmacy  = CreatePharm {getUsers :: [User], getMeds :: [Med]}

data User      = CreateUser  {
        firstName  :: Name,
        lastName   :: Name,
        userName   :: Username, 
        previlage  :: Previlage,
        salary     :: Salary, 
        status     :: Status
    } deriving (Show, Eq)

data Med       = CreateMed   {getName :: Name, getAmount :: Int, getPrice :: Float} deriving (Show, Eq)

data Status    = Onshift | OffShift | OnVacation | NotEmployed deriving (Show, Eq)

data Previlage = Admin   | Normal deriving (Show, Eq)


-- Pharmacy will have, login page, admin page, user page.
-- Loginpage, asks for username and password and checks from the file (password save using hash)
-- Admin page, CRUD operation on meds, CRUD operations on Users, logout
-- User page, See lists of meds
-- All users, Sell medicine, Add medicine