{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Except
import Data.Trie
import Data.Maybe
import Data.String
import Data.Monoid
import Data.Foldable hiding (forM_, mapM_)
import Game.Boggle
import Game.Boggle.Bot
import System.Random
import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.RWS
import Network.IRC.Bot.STM
import Network.Connection
import Network.TLS
import System.Environment
import Data.ConfigFile as CF
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
    channel :: BS.ByteString,
    password :: Maybe BS.ByteString,
    insecure :: Bool
} deriving (Read, Show, Eq, Ord)

parseConfig :: (MonadError CPError m, MonadIO m, Alternative m, Monad m) =>
    ConfigParser -> m Config
parseConfig conf = Config <$>
    get conf "DEFAULT" "host" <*>
    get conf "DEFAULT" "port" <*>
    get conf "DEFAULT" "ident" <*>
    get conf "DEFAULT" "channel" <*>
    (get conf "DEFAULT" "password" <|> return Nothing) <*>
    (get conf "DEFAULT" "insecure_mode" <|> return False)

instance Monoid CPErrorData where
    mempty = OtherProblem "mempty Error"
    mappend (OtherProblem "mempty Error") y = y
    mappend x _ = x

bot :: Trie -> Config ->
    Bot (Bool, Bool, GameState) Message Response ()
bot t conf = do
    let ch = channel conf

    pingBot
    
    (init, logged, _) <- S.get

    unless init $ do
        writeOut (Nick (ident conf))
        writeOut (Login (ident conf))
        _1 .= True

        case password conf of
            Just pw -> do
                writeOut (SendMsg "NickServ" $ "IDENTIFY " <> pw)
                _2 .= False
                timeout 15000000
            _ -> do
                _2 .= True
                writeOut (JoinChannel ch)

    unless logged $ do
        handleTimeout $ do
            writeOut (JoinChannel ch)
            _2 .= True

    when logged $ botState _3 $ boggleBot t ch

runBotIO :: Trie -> Config -> IO ()
runBotIO t conf = do
    let c' = if insecure conf
        then (tlsClientConfig (port conf) (host conf)) {
                tlsClientTLSSettings = TLSSettingsSimple True True True
            }
        else tlsClientConfig (port conf) (host conf)
    runTLSClient c' (handler conf t)

handler :: Config -> Trie -> AppData -> IO ()
handler conf t app = do
    g <- newStdGen
    (inp, outp) <- startBot (False, False, (g, Nothing)) (bot t conf)

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
                    liftIO $ atomically $ writeTChan inp msg
                    tcSink

    forkIO $ tcSource $$ appSink app
    appSource app =$= conduitParser parseMessage $$ tcSink

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
            runBotIO d conf

