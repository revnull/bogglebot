{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Data.Monoid
import Game.Boggle.Bot
import Game.Boggle (Trie)
import System.Random
import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.STM
import Network.Connection
import System.Environment
import System.Directory
import Data.ConfigFile as CF
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Data.List as L
import Data.Maybe
import Data.FixFile
import qualified Data.FixFile.Trie.Light as T

data Config = Config {
    host :: BS.ByteString,
    port :: Int,
    ident :: BS.ByteString,
    chan :: BS.ByteString,
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

bot :: Config -> StdGen -> IRCBot (Ref Trie) ()
bot conf g = do
    let ch = chan conf

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

    boggleBot ch g

runBotIO :: FixFile (Ref Trie) -> Config -> IO ()
runBotIO t conf = do
    let c' = if insecure conf
        then (tlsClientConfig (port conf) (host conf)) {
                tlsClientTLSSettings = TLSSettingsSimple True True True
            }
        else tlsClientConfig (port conf) (host conf)
    runTLSClient c' (handler conf t)

handler :: Config -> FixFile (Ref Trie) -> AppData -> IO ()
handler conf t app = do
    g <- newStdGen
    (inp, outp) <- startBot t (bot conf g)

    let tcSource = forever $ do
            resp <- liftIO . atomically $ readTChan outp
            yield (BSL.toStrict $ encodeResponse resp)
            yield "\n"
        tcSink = do
            msg <- await
            case msg of
                Nothing -> return ()
                Just (_, msg') -> do
                    liftIO $ atomically $ writeTChan inp msg'
                    tcSink

    void $ forkIO $ tcSource $$ appSink app
    appSource app =$= conduitParser parseMessage $$ tcSink

buildDict :: FilePath -> FilePath -> IO (FixFile (Ref Trie))
buildDict src dst = dictFile where
    dictFile = do
        ex <- doesFileExist dst
        if ex then openFixFile dst else build
    build = do
        Prelude.putStrLn $ "Building dictionary database."
        f <- T.createTrieFile dst
        ws <- (catMaybes . fmap sanitize . BSL.split 10) <$> BSL.readFile src
        forM_ (chunk ws) $ \ch -> writeTransaction f $ 
            forM_ ch $ \w -> T.insertTrieT w ()
        vacuum f
        Prelude.putStrLn $ "Finished building dictionary database."
        return f
    chunk [] = []
    chunk l =
        let (c, r) = L.splitAt 256 l
        in c : chunk r

main :: IO ()
main = do 
    [conf'] <- getArgs
    econf <- runExceptT $ do
        c <- join $ liftIO $ readfile emptyCP conf'
        parseConfig c
    case econf of
        Left err -> print err
        Right conf -> do
            print conf
            d <- buildDict "data/american.dict" "data/dict.db"
            runBotIO d conf

