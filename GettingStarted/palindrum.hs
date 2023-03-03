main = interact respondPalindrom

respondPalindrom :: String -> String
respondPalindrom = unlines . map (\xs -> if isPalindrom xs then "Palindrom" else "Not Palindrom") . lines
    where isPalindrom xs = xs == reverse xs
