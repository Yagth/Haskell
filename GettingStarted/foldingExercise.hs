rev :: [a] -> [a]
rev = foldl (flip(:)) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x]:acc) [[]]