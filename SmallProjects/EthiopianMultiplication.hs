module EthiopianMultiplication where

mult :: (Num a, Integral a) => a -> a -> a
mult 1 n = n
mult m n
 | odd m = n + mult (m `div` 2) (n * 2)
 | otherwise = mult (m `div` 2) (n * 2)