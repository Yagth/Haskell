-- let maximumOfFive = (max 5)
multthree :: (Num a) => a->a->a->a
multthree x y z = x*y*z

multWithFive :: (Num a) => a->a->a
multWithFive = multthree 5

multWithTwenty :: (Num a) => a->a
multWithTwenty = multWithFive 4

compareWithHundrud :: (Num a, Ord a) => a->Ordering
compareWithHundrud = (`compare` 100)

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c)->b->a->c 
flip' f x y = f y x

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

--Filter with list comprehnsion 
filter' :: (a->Bool)->[a]->[a]
filter' f xs = [ a | a <- xs, f a]

--Filter with recursion
filter'' :: (a->Bool)->[a]->[a]
filter'' _ [] = []
filter'' f (x:xs)
    | f x = x:filter'' f xs 
    | otherwise = filter'' f xs

maximum' :: (Ord a )=> [a]->a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a)  => [a] -> a 
product' = foldl1 (*)

filter''' :: ( a -> Bool ) -> [a] -> [a]
filter''' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldl1 (\acc x -> acc)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

last'' = scanl1 (\_ x -> x)

sumOfSqrt :: Int
sumOfSqrt = length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1

oddSquareSum :: [Int] -> Int
oddSquareSum = sum . takeWhile (<10000) . map (^2) . filter odd