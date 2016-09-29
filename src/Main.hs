{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Except
import Data.Trie
import Data.Maybe
import Data.String
import Data.Monoid
import Data.Foldable hiding (forM_, mapM_)
import Game.Boggle
import System.Random
import Network.IRC
import Network.TLS
import System.Environment
import Data.ConfigFile
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Data.IORef
import Data.List as L
import Data.Function

data Config = Config {
    host :: BS.ByteString,
    port :: Int,
    ident :: BS.ByteString,
    channel :: BS.ByteString
} deriving (Read, Show, Eq, Ord)

parseConfig :: (MonadError CPError m, MonadIO m, Alternative m, Monad m) =>
    ConfigParser -> m Config
parseConfig conf = Config <$>
    get conf "DEFAULT" "host" <*>
    get conf "DEFAULT" "port" <*>
    get conf "DEFAULT" "ident" <*>
    get conf "DEFAULT" "channel"

instance Monoid CPErrorData where
    mempty = OtherProblem "mempty Error"
    mappend (OtherProblem "mempty Error") y = y
    mappend x _ = x

bot :: Trie -> Config -> IO ()
bot t conf = do
    let c' = tlsClientConfig (port conf) (host conf)
    runTLSClient c' (handler conf t)

handler :: Config -> Trie -> AppData -> IO ()
handler conf t app = do
    inp <- newTChanIO
    outp <- newTChanIO
    atomically $ do
        writeTChan outp (Nick (ident conf))
        writeTChan outp (Login (ident conf))
        writeTChan outp (JoinChannel (channel conf))
    g <- newStdGen
    gs <- newTVarIO (g, Nothing)
    let tcSource = forever $ do
            resp <- liftIO . atomically $ readTChan outp
            yield (BSL.toStrict $ encodeResponse resp)
            yield "\n"
        tcSink = do
            msg <- await
            liftIO $ print msg
            case msg of
                Nothing -> return ()
                Just (_, msg) -> botHandler msg >> tcSink
        botHandler (Ping m) = liftIO . atomically $ writeTChan outp (Pong m)
        botHandler m = liftIO $ do
            ng <- atomically $ do
                gs' <- readTVar gs
                let ((ng, msgs), gs'') =
                        S.runState (playGame t (channel conf) m) gs'
                mapM_ (writeTChan outp) msgs
                writeTVar gs gs''
                return ng
            when ng . void . forkIO $ do
                threadDelay 120000000
                atomically $ writeTChan outp
                    (SendMsg (channel conf) "1 Minute Remaining")
                threadDelay 60000000
                atomically $ do
                    gs' <- readTVar gs
                    let (Just (scores, missed), gs'') = S.runState endGame gs'
                    writeTVar gs gs''
                    writeTChan outp (SendMsg (channel conf) "Time's up!")
                    let scores' = L.reverse $ sortBy (compare `on` snd) scores
                    forM_ scores' $ \(p, s) -> do
                        let msg = p <> " : " <> fromString (show s)
                        writeTChan outp (SendMsg (channel conf) msg)
                    forM_ (chunkWords missed) $ \miss -> do
                        writeTChan outp (SendMsg (channel conf) ("Missed Words: " <>
                            BS.intercalate ", " miss))
    forkIO $ tcSource $$ appSink app
    appSource app =$= conduitParser parseMessage $$ tcSink

chunkWords :: [BS.ByteString] -> [[BS.ByteString]]
chunkWords = ($ []) . chunk' 0 id where
    chunk' 0 _ [] = id
    chunk' _ f [] = (f []:)
    chunk' c f l@(x:xs)
        | c > 100 = (f []:) . chunk' 0 id l
        | otherwise = chunk' (c + BS.length x) (f . (x:)) xs

playGame :: Trie -> BS.ByteString -> Message ->
    S.State GameState (Bool, [Response])
playGame t ch (PrivMsg (User usr _) ch' txt)
    | ch == ch' = do
        r <- gameRunning 
        case (r, txt == "Boggle Time", txt == "!board") of
            (True, _, showBoard) -> do
                let bs = catMaybes . fmap sanitize . BS.split 32 $ txt
                mapM_ (scoreWord usr) bs
                if showBoard
                    then do
                        Just b <- getBoard
                        return (False, fmap (SendMsg ch) (boardLines b))
                    else return (False, [])
            (False, True, _) -> do
                newGame t
                (_, Just (b, ws, _)) <- S.get
                let xs = fmap (SendMsg ch) (boardLines b)
                    btmsg = SendMsg ch "It's Boggle Time!"
                    msmsg = SendMsg ch ("Maximum Score " <> fromString (show ms))
                    ms = getSum $ foldMap (Sum . wordValue) ws
                return (True, btmsg:msmsg:xs)
            _ -> return (False, [])
    | otherwise = return (False, [])
playGame _ _ _ = return (False, [])

main = do 
    [conf'] <- getArgs
    econf <- runExceptT $ do
        c <- join $ liftIO $ readfile emptyCP conf'
        parseConfig c
    case econf of
        Left err -> print err
        Right conf -> do
            d <- readDict "data/american.dict"
            Prelude.putStrLn $ "Total Word Count: " ++
                show (L.length (fullDict d))
            bot d conf
    
