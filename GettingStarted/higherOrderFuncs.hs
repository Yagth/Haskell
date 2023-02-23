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