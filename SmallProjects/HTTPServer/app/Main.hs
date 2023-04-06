{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main where

import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM
import qualified Web.Scotty as S
import qualified Lucid as H
import Control.Monad.IO.Class (liftIO)

type Html = H.Html ()

main :: IO()
main = do
    putStrLn "Hello!"
    listVar <- STM.newTVarIO ([] :: [T.Text])
    S.scotty 8082 $ do
        S.get  "/" $ do
            list <- liftIO $ STM.readTVarIO listVar
            html $ do
                H.ul_ $ do
                    H.form_ [ H.method_ "POST", H.action_ "/delete"] $
                        flip mapM_ (reverse $ zip [0..] list) $ \(index :: Int, entry) -> do
                            H.li_ $ do
                                H.toHtml entry
                                H.button_ 
                                    [ H.type_ "submit"
                                    , H.name_ "index"
                                    , H.value_ (T.pack (show index))
                                    ] 
                                    "Delete"
                H.form_ [H.method_ "POST", H.action_ "/add"] $ do
                    H.input_ [H.name_ "entry", H.placeholder_ "New entry...", H.autofocus_]
                    H.button_ [H.type_ "submit"] "Add"

        S.post "/add" $ do
            text <- S.param "entry"
            liftIO $ STM.atomically $ STM.modifyTVar' listVar (text :)
            S.redirect "/"
        S.post "/delete" $ do
            index <- S.param "index"
            liftIO $ STM.atomically $ STM.modifyTVar' listVar (removeAt index)
            S.redirect "/"

html :: Html -> S.ActionM ()
html = S.html . H.renderText . H.doctypehtml_

removeAt :: Int -> [a] -> [a]
removeAt index list
    | index == 0,
      (_:xs) <- list
      = xs
    | x:xs <- list = x : removeAt (index - 1) xs
    | otherwise = []