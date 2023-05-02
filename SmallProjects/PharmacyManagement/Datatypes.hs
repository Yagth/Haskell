module Datatypes where


type Name     = String
type Username = String
type Password = String
type Salary   = Float
type EndDate  = String

data Pharmacy = CreatePharm {getUsers :: [User], getMeds :: [Med]}

data User     = CreateUser  {
        getFirstName :: Name,
        getLastName  :: Name,
        getUserName  :: Username, 
        getSalary    :: Salary, 
        getStatus    :: Status
    }

data Med      = CreateMed   {getName :: Name, getAmount :: Int, getPrice :: Float}

data Status   = Onshift | OffShift | OnVacation EndDate


-- Pharmacy will have, login page, admin page, user page.
-- Loginpage, asks for username and password and checks from the file (password save using hash)
-- Admin page, CRUD operation on meds, CRUD operations on Users, logout
-- User page, See lists of meds
-- All users, Sell medicine, Add medicine