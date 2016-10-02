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
import qualified Network.IRC.Bot as B
import Network.IRC.Bot.STM
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

startBots :: Trie -> StdGen -> BS.ByteString -> B.Bot () ()
startBots t g ch = do
    B.fork () B.pingBot
    B.fork (g, Nothing) (B.runChannel ch $ boggleBot ch t)

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
    newBot inp outp () $ startBots t g (channel conf)
    let tcSource = forever $ do
            resp <- liftIO . atomically $ readTChan outp
            yield (BSL.toStrict $ encodeResponse resp)
            yield "\n"
        tcSink = do
            msg <- await
            liftIO $ print msg
            case msg of
                Nothing -> return ()
                Just (_, msg) -> do
                    liftIO $ atomically $ writeTChan inp (Right msg)
                    tcSink

    forkIO $ tcSource $$ appSink app
    appSource app =$= conduitParser parseMessage $$ tcSink

chunkWords :: [BS.ByteString] -> [[BS.ByteString]]
chunkWords = ($ []) . chunk' 0 id where
    chunk' 0 _ [] = id
    chunk' _ f [] = (f []:)
    chunk' c f l@(x:xs)
        | c > 100 = (f []:) . chunk' 0 id l
        | otherwise = chunk' (c + BS.length x) (f . (x:)) xs

boggleBot :: BS.ByteString -> Trie -> B.Bot GameState ()
boggleBot ch t = forever bb where
    bb = do
        r <- gameRunning
        B.await >>= handle r . cap
    cap (Just (PrivMsg u c m)) = Just $ PrivMsg u c $ BS.map capW8 m
    cap m = m
    capW8 c
        | c >= 97 && c <= 122 = c - 32
        | otherwise = c
    sendMsg = B.respond . SendMsg ch
    handle True (Just (PrivMsg u _ "!BOARD")) = do
        (_, Just (b, _, _, _)) <- S.get
        forM_ (boardLines b) $ \l -> do
            sendMsg l
    handle True (Just (PrivMsg (User usr _) _ m)) = do
        let bs = catMaybes . fmap sanitize . BS.split 32 $ m
        mapM_ (scoreWord usr) bs
    handle True Nothing = do
        (g, Just (b, ws, ss, w)) <- S.get
        if w
            then do
                Just (scores, missed) <- endGame
                sendMsg "Time's up!"
                let scores' = L.reverse $ sortBy (compare `on` snd) scores
                forM_ scores' $ \(p, s) -> do
                    sendMsg (p <> " : " <> fromString (show s))
                forM_ (chunkWords missed) $ \miss -> do
                    sendMsg ("Missed Words: " <> BS.intercalate ", " miss)
            else do
                S.put (g, Just (b, ws, ss, True))
                sendMsg "1 Minute Remaining"
                B.timeout 60000000
    handle _ (Just (PrivMsg u _ "BOGGLE TIME")) = do
        newGame t
        B.timeout 120000000
        (_, Just (b, ws, _, _)) <- S.get
        sendMsg "It's Boggle Time!"
        let ms = getSum $ foldMap (Sum . wordValue) ws
        sendMsg ("Maximum Score " <> fromString (show ms))
        forM_ (boardLines b) $ \l -> do
            sendMsg l
    handle _ _ = return ()

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

