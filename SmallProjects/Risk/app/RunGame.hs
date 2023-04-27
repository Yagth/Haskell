module RunGame where

import Risk
import Control.Monad.Random

runBattle:: Battlefield -> IO Battlefield
runBattle bf = evalRandIO $ battle bf

runInvade :: Battlefield -> IO Battlefield
runInvade bf = evalRandIO $ invade bf

runProb :: Battlefield -> IO Double
runProb bf = evalRandIO $ successProb bf