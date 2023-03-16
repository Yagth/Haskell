import Data.Char (isDigit)
import System.Environment (getArgs)
import Data.List (intersperse)
solveRpn :: String -> Float
solveRpn = head . foldl calculate [] . words

calculate :: [Float] -> String -> [Float]
calculate (x:y:ys) "+" = x + y : ys
calculate (x:y:ys) "-" = y - x : ys
calculate (x:y:ys) "*" = x * y : ys
calculate (x:y:ys) "/" = y / x : ys
calculate xs numberString = read numberString : xs

toRpn :: String -> String
toRpn xs = filter (`notElem` "()") rpnWithBrackets
    where rpnWithBrackets = reverse $ pushToStack xs "" ""

pushToStack :: String -> String -> String -> String
pushToStack [] expression stack = popAllStack expression stack
pushToStack (x:xs) expression stack 
    | isDigit x = pushToStack restExpr (' ':(reverse . show) digit ++ expression) stack
    | x == ')' = pushToStack xs newExpr newStack 
    | null stack = pushToStack xs expression (x:stack)
    | precedence (head stack) >= precedence x = pushToStack xs (' ':head stack: expression) (x: tail stack)
    | otherwise = pushToStack xs expression (x:stack)
    where (newExpr, newStack) = popUntilOpenBracket expression stack
          [(digit, restExpr)] = reads (x:xs) :: [(Integer, String)]

popAllStack :: String -> String -> String
popAllStack expr [] = expr
popAllStack expr [x] = ' ':x:expr
popAllStack expr (x:ys) = popAllStack (' ':x:expr) ys

popUntilOpenBracket :: String -> String -> (String, String)
popUntilOpenBracket expr [] = (expr, "")
popUntilOpenBracket expr ['('] = (expr, "")
popUntilOpenBracket expr ('(':xs) = (expr,xs)
popUntilOpenBracket expr (x:ys) = popUntilOpenBracket (' ':x:expr) ys

precedence :: Char -> Int
precedence '+' = 2
precedence '*' = 3
precedence '/' = 3
precedence '-' = 2
precedence '(' = 5
precedence ')' = 0
precedence xs = 1

rpnMode :: [String] -> IO ()
rpnMode expression = do
    let rpn = toRpn (unwords expression)
        solved = solveRpn rpn
    putStrLn $ "Solution: " ++ show solved

main ::IO ()
main = do
    args <- getArgs
    let action = if null args 
                    then failure args
                    else let (command:arg) = args; result = lookup command modes;
                              action' = case result of
                                Nothing -> help arg
                                (Just function) -> function arg
                              in action'
    action

modes :: [(String, [String]-> IO ())]
modes = [
        ("--rpn", rpnMode),
        ("--add", normalMode "+"),
        ("--sub", normalMode  "-"),
        ("--mult", normalMode "*"),
        ("--div", normalMode "/"),
        ("--help", help)
        ]

normalMode ::String -> [String] -> IO()
normalMode opp [numStr1, numStr2] = do
    putStrLn ("Solution: \n\t "++numStr1 ++ ' ':opp ++ ' ':numStr2 ++ " = "++show sum)
    where sum = binaryOpp opp (read numStr1)  (read numStr2)

binaryOpp :: (Fractional a) => String -> a -> a -> a
binaryOpp opp num1 num2 = case opp of
        "+" -> num1 + num2
        "-" -> num1 - num2
        "/" -> num1 / num2
        "*" -> num1 * num2

failure ::[String] -> IO ()
failure _ = do
        
    putStrLn "Invalid arguement detected. Check the help for more information\n"
    help []

help ::[String] -> IO ()
help _ = do
    let helpStr = "Help menu :\n"++
                  " | --add    <firstNumber>  <secondNumber> : To add two numbers\n"++
                  " | --sub    <firstNumber>  <secondNumber> : To subtract two numbers\n" ++
                  " | --mult   <firstNumber>  <secondNumber> : To mutliply two numbers\n" ++
                  " | --div    <firstNumber>  <secondNumber> : To divide two numbers\n"++
                  " | --rpn    <expresion>                   : To perform mulitple operations at the same time.\n"++
                  "\t\t\t\t\t    Use brackets to imply prefered preference\n"++
                  " | --help                                 : To see the list of available commands\n"
    putStrLn helpStr