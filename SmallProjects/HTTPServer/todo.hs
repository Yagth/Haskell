{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM
import qualified Web.Scotty
import qualified Lucid as H
import Control.Monad.IO.Class (liftIO)

type HTML = H.Html ()

main :: IO()
main = do
    putStrLn "Hello!"
    listVar <- STM.newTVarIO []
    S.scotty 8080 $ do
        S.get  "/" $ do
            list <- STM.readTVarIO listVar
            pure ()
        S.post "/add" $ do
            text <- S.param "entry"
            pure ()
        S.post "/delete" $ do
            index <- S.param "index"
            pure ()
