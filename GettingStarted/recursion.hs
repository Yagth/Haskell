maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x 
    | otherwise = maxTail 
    where maxTail = maximum' xs

maximum'' :: (Ord a)=> [a]->a 
maximum'' [] = error "Empty list"
maximum'' [x] = x 
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i , Ord i ) => i->a->[a]
replicate' n x 
    | n<=0 = []
    | otherwise = x:(replicate' (n-1) x)

take' :: (Num i, Ord i) => i->[a]->[a]
take' _ [] = []
take' n _ 
    | n<=0 = []
take' n (x:xs) = x: take' (n-1) xs

reverse' :: [a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a->[a]
repeat' x = x:repeat' x

zip' :: [a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

elem' ::(Eq a) => a->[a]->Bool
elem' _ [] = False
elem' y (x:xs)
    |y==x = True
    |otherwise = y `elem` xs

quicksort :: (Ord a) => [a]->[a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerlist = quicksort [a | a <- xs , a<=x]
        biggerlist = quicksort [a | a <- xs, a > x ]
    in 
        smallerlist ++ [x] ++ biggerlist