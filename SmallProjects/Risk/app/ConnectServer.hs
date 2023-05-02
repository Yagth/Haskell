module ConnectServer where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Exception as E
import Control.Monad (unless, forever, void, join, Functor (fmap))
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import RunGame
import Risk
import Control.Concurrent.STM (newTVar)

data MessageType = Prob Double | GameState Battlefield | Unknown String deriving(Show)

main :: IO ()
main = do
  putStrLn "Server running on port 3000"
  battlefield' <- newTVarIO $ Battlefield {attackers=10, defenders = 10}
  runTCPServer Nothing "3000" (talk battlefield')

talk bf s = do
        msg <- recv s 1024
        unless (C.unpack msg == "/quit") $ do
          result <- case C.unpack msg of
            "/attack"   -> GameState <$> updateTVar bf runBattle
            "/prob"     -> do
              currentBF <- readTVarIO bf
              Prob <$> runProb currentBF
            "/invade"   -> GameState <$> updateTVar bf runInvade
            "/pass"     -> GameState <$> updateTVar bf return 
            "/stat"     -> GameState <$> readTVarIO bf
            _           -> return $ Unknown "Unknown Commnad"
          sendAll s (C.pack (show result))
          talk bf s

updateTVar bfState f = do
         currentBF <- readTVarIO bfState
         GameState <$> f currentBF

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    -- This function is used to create a connection given the 
    --parameters needed to do so.
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = do
      forever $ do

        --Accept new connection
        E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do

           --Handle the new connection
            void $
              forkFinally (server conn) (const $ do
                gracefulClose conn 5000)