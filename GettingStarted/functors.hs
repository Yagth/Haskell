import Control.Applicative (Applicative(liftA2))
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

newtype Pair b a = Pair {getPair :: (a,b)}

instance Functor (Pair c) where 
    fmap f (Pair (x,y)) = Pair (f x, y)