{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module ConnectClient (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    putStr "msg:"
    client <- getLine
    sendAll s $ C.pack client
    msg <- recv s 1024
    if S.null msg
        then do 
            return ()
    else do
        putStr "Received: "
        C.putStrLn msg
        main
-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock