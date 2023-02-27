class Eq' a where
    equal :: a -> a-> Bool
    notEqual :: a -> a-> Bool

data PaymentMethod = Card | Bank | CC 

type User = (String, PaymentMethod)

instance Eq' PaymentMethod where 
    equal Card Card = True
    equal Bank Bank = True 
    equal CC CC = True
    equal _ _ = False

    notEqual Card Card = False 
    notEqual Bank Bank = False 
    notEqual CC CC = False
    notEqual _ _ = True

--Using recursive definition for the above implementation

class Eq'' a where
    equal', notEqual' :: a->a->Bool 
    
    equal' x y = not $ notEqual' x y
    notEqual' x y = not $ equal' x y

instance Eq'' PaymentMethod where
    equal' Card Card = True 
    equal' Bank Bank = True 
    equal' CC CC = True 
    equal' _ _ = False


data CryptoCurrency = Etherium | Bitcoin | Other

instance Eq'' CryptoCurrency where 
    equal' Etherium Etherium = True 
    equal' Bitcoin Bitcoin = True 
    equal' Other Other = True 
    equal' _ _ = False

data Box a = Empty | Has a deriving Show
newtype Country = Country {name::String}

instance (Eq'' a) => Eq'' (Box a) where 
    equal' (Has x) (Has y) = equal' x y
    equal' Empty Empty = True 
    equal' _ _ = False

class WeAccept a where 
    weAccept :: a -> Bool 



instance WeAccept PaymentMethod where 
    weAccept x = case x of 
        Bank -> False
        _ -> True    
instance WeAccept CryptoCurrency where
    weAccept x = case x of 
        Etherium -> True 
        Bitcoin -> True 
        Other -> False

instance (WeAccept a) => WeAccept (Box a) where 
    weAccept (Has x) = weAccept x
    weAccept Empty = False

instance WeAccept Country where 
    weAccept c = case name c of 
        "Somalia" -> False 
        _ ->  True 

fancyFunc :: (WeAccept a) => a -> String 
fancyFunc x = if weAccept x then "Do something fancy" else "Do nothing"