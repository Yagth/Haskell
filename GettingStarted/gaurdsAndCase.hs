bmiTell :: (RealFloat a) => a->String
bmiTell bmi 
    | bmi<=18.5 = "When was the last time you ate food. 2004?"
    | bmi<=25 = "You seem health. But are you okay?"
    | bmi<=30 = "You are on your way to beat the elephant"
    | otherwise = "Congradulations! You beat the elephant"

max' :: (Ord a) => a->a->a
max' a b 
    | a<b = b
    | otherwise = a

min' :: (Ord b) => b->b->b 
min' a b | a<b = a | otherwise = b

myCompare :: (Ord b) => b->b->String
a `myCompare`  b | a<b = "LT" | a>b = "GT" | otherwise = "EQ"

bmiTell' :: (RealFloat a ) => a->a->String
bmiTell' w h 
    | bmi <= skinny = "You are skinny"
    | bmi <= normal = "You are normal"
    | bmi <= fat = "You are fat"
    | otherwise = "You are obesse"
    where bmi = w / h^2
          (skinny, normal, fat, _) = (18.5, 25.0, 30.0, 40.0)

bmis :: (RealFloat b) => [(b,b)]->[b]
bmis xs = [bmi w h | (w,h) <- xs]
            where bmi w h = w / h^2

intials :: String->String->String
intials firstName secondName = [f] ++ "." ++ [s]++"."
                            where (f:_) = firstName
                                  (s:_) = secondName