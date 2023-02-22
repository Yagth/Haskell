factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial(n-1)

fib :: (Integral a) => a -> a
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1+y1, x2+y2)

first :: (a,b,c) -> a 
first (a,_,_) = a 

second :: (a,b,c) -> b 
second (_, b, _) = b 

third :: (a,b,c) -> c 
third (_,_,c) = c 

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> a
tail' [x,y] = y
tail' (_:xs) = tail' xs

tell :: (Show a) => [a]->String
tell [] = "The list is empty"
tell [x] = "The list has one element: "++show x
tell [x,y,_] = "The list is long with first element: "++show x ++" second element: "++show y
tell [x,y] = "The list has two elements: "++show x ++" and "++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

captial :: String -> String
captial "" = error "Whoops, empty string found"
captial all@(x:_) = "The captial of "++all++" is: "++[x]

