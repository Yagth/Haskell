import System.Random
import Control.Monad (when, unless)

main = do
    gen <- getStdGen

    askRandom gen

askRandom :: StdGen -> IO ()
askRandom gen = do
   let (randNumber, newgen) = randomR (1,10) gen :: (Int, StdGen)

   putStrLn "What number am I thinking between 1 and 10?"
   numberstr <- getLine

   when (not $ null numberstr) $ do
    let list = reads numberstr

    case list of
        [] -> putStrLn "Invalid input"
        [(number, _)] -> if  number == randNumber
        then putStrLn "Yeah. That's correct!"
        else putStrLn "Nah that is not right!"

    askRandom newgen
    

    
    