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
    password :: BS.ByteString,
    insecure :: Bool
} deriving (Read, Show, Eq, Ord)

parseConfig :: (MonadError CPError m, MonadIO m, Alternative m, Monad m) =>
    ConfigParser -> m Config
parseConfig conf = Config <$>
    get conf "DEFAULT" "host" <*>
    get conf "DEFAULT" "port" <*>
    get conf "DEFAULT" "ident" <*>
    get conf "DEFAULT" "channel" <*>
    (get conf "DEFAULT" "password" <|> return "") <*>
    (get conf "DEFAULT" "insecure_mode" <|> return False)

instance Monoid CPErrorData where
    mempty = OtherProblem "mempty Error"
    mappend (OtherProblem "mempty Error") y = y
    mappend x _ = x

bot :: Trie -> Config -> StdGen -> IRCBot ()
bot t conf g = do
    let ch = channel conf

    fork $ pingBot
    
    writeOut (Nick (ident conf))
    writeOut (Login (ident conf))

    let comm4 (Command _ 4 _) = Just ()
        comm4 _ = Nothing
    waitFor comm4

    let pw = password conf
    when (not $ BS.null pw) $ do
        writeOut (SendMsg "NickServ" $ "IDENTIFY " <> pw)
        timeout 10000000
        waitFor' (\i -> guard (isTimeout i) >> return ())

    writeOut (JoinChannel ch)

    boggleBot t ch g

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
    (inp, outp) <- startBot (bot t conf g)

    let tcSource = forever $ do
            resp <- liftIO . atomically $ readTChan outp
            yield (BSL.toStrict $ encodeResponse resp)
            yield "\n"
        tcSink = do
            msg <- await
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
            print conf
            d <- readDict "data/american.dict"
            Prelude.putStrLn $ "Total Word Count: " ++
                show (L.length (fullDict d))
            runBotIO d conf

