describeList :: [b] -> String
describeList xs = case xs of [] -> "List is empty"
                             [x] -> "List is singlton"
                             xs -> "List is longer"

describeList' :: [b]->String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "singlton"
          what xs = "Longer" 

describeList'' :: [b]->String
describeList'' xs = "The list is " ++ case xs of [] -> "empty"
                                                 [x] -> "singlton"
                                                 xs -> "Longer"