import Control.Monad.Trans.Writer

double :: (Num a) => a -> Maybe a
double x = Just x >>= (\y -> Just $ y * 2)

failingHa :: Maybe Char
failingHa = do
    (x:xs) <- Just "Hello"
    return x

type Bird = Int
type Pole = (Bird, Bird)

leftBird :: Bird -> Pole -> Maybe Pole
leftBird n (x,y)
    | abs (x + n - y) < 4 = Just (x+n, y)
    | otherwise = Nothing

rightBird :: Bird -> Pole -> Maybe Pole
rightBird n (x, y)
    | abs (y+n - x) < 4 = Just (x,y+n)
    |otherwise = Nothing

walk :: Maybe Pole
walk = return (0,0) >>= leftBird 1 >>= rightBird 1 >>= rightBird 2

walk' :: Maybe Pole
walk' = case leftBird 1 (0,0) of
            Nothing -> Nothing
            Just first -> case rightBird 1 first of
                Nothing -> Nothing
                Just second -> rightBird 2 second

walk'' :: Maybe Pole
walk'' = do
    first <- leftBird 1 (0,0)
    second <- rightBird 1 first
    rightBird 2 second

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard 
    [
    (c-1, r+2), (c+1, r+2), 
    (c-1, r-2), (c+1, r-2), 
    (c+2, r-1), (c+2, r+1), 
    (c-2, r+1), (c-2, r-1)
    ]
    where onBoard (x,y) = x `elem` [1..8] && y `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 (c,r) = moveKnight (c,r) >>= moveKnight >>= moveKnight

canMoveIn3 :: KnightPos -> KnightPos -> Bool
canMoveIn3 start end = end `elem` in3 start
  
theThreeMoves :: KnightPos -> KnightPos -> [KnightPos]
theThreeMoves start end 
    | canMoveIn3 start end =  do
        let first  = moveKnight start
            second = first >>= moveKnight
            sec   = filter (\x -> end `elem` moveKnight x ) second
            fir   = filter (`elem` first) sec
        fir
        -- first'  <- filter (second' == ) (moveKnight first)
        -- [start, first', second', end]
    | otherwise = []

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (<>) :: DiffList a -> DiffList a -> DiffList a
    (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where  
    mempty :: DiffList a
    mempty = DiffList ([] ++)  

gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  