import Control.Monad.Writer

main:: IO ()
main = putStrLn "Nothing in here yet"

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Accepted number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 4
    return (a * b)

gcd' :: Int -> Int -> Int
gcd' a b | b == 0 = a | otherwise = gcd' b (a `mod` b)

gcdWithLog :: Int -> Int -> Writer [String] Int
gcdWithLog a b 
    | b == 0 = do 
        tell ["Finished with: " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcdWithLog b (a `mod` b)