import Data.Sequence (Seq)
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf )

data Employee = Employee { name :: String, salary :: Float } | Not {name :: String} deriving Show

--Creating a new instance of a data type
matt = Employee "Matt" 12.3

--Updating the previouse instance's specific field
newMatt = matt {salary = 45.6}

combSalary :: [Employee] -> Float
combSalary = foldl (\acc e -> salary e + acc) 0

--We can have pattern matching like this for record syntax
getNameOrSalary Employee {name=n} = print n

data Tweet = Tweet { contents :: String,  like :: Int, comments :: [Tweet] } deriving Show

tweet = Tweet "Hello there I hate is here" 5
    [ Tweet "Yeah me too" 0 [], 
      Tweet "So do I" 2  
        [ Tweet "Are you guys serious" 3 []]
    ]

engagement :: Tweet -> Int 
engagement Tweet {like = l, comments = c} = l + length c + sum (map engagement c)

--Defining a new infix operator 
--that is right associative constructor 
infixr 5 :->
data Sequence a = EmptyS | a :-> (Sequence a) deriving Show --Reimplementation of the list

elemSeq :: (Eq a) => a -> Sequence a -> Bool
elemSeq _ EmptyS = False
elemSeq x (y :-> ys) = x == y || elemSeq x ys

