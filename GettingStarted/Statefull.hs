import System.Random
import Control.Monad.State

type Stack = [Int]

push :: Int -> State Stack ()
push a = state $ \s -> ((), a:s)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

doSomeStuff :: State Stack Int
doSomeStuff = do
    push 3
    pop
    pop
    push 4
    pop

doOtherStuff :: State Stack ()
doOtherStuff = do
    push 3
    a <- pop
    push 2

    if a == 3
        then put [1,2,3,4]
        else put [2,3,4,5]

moreStuff :: State Stack ()
moreStuff = do
    doOtherStuff

randomSt :: (Random a, RandomGen g) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool) 
threeCoins = do
    firstCoin <- randomSt
    secondCoin <- randomSt
    thirdCoin <- randomSt
    return (firstCoin, secondCoin, thirdCoin)

