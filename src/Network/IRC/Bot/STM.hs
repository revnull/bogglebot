{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Bot.STM (
                            interpret
                           ,newBot
                           ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Free.Church
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import Network.IRC
import Network.IRC.Bot

type InChan = TChan (Either ThreadId Message)
type OutChan = TChan Response

type IOBot s a = R.ReaderT (InChan, OutChan) (S.StateT s IO) a

interpret :: Bot s a -> IOBot s a
interpret = foldF int . runBot where
    int a@(Await f) = do
        chan <- R.asks fst
        msg <- liftIO . atomically $ readTChan chan
        case msg of
            Left tid -> do
                tid' <- liftIO myThreadId
                if (tid == tid')
                    then return (f Nothing)
                    else int a
            Right m -> return . f $ Just m
    int (Respond m a) = do
        chan <- R.asks snd
        liftIO . atomically $ writeTChan chan m
        return a
    int (Timeout i a) = do
        tid <- liftIO $ myThreadId
        chan <- R.asks fst
        liftIO . forkIO $ do
            threadDelay i
            atomically $ writeTChan chan (Left tid)
        return a
    int (Get f) = f <$> S.get
    int (Put s a) = S.put s >> return a
    int (Fork s b a) = do
        (inp, outp) <- R.ask
        liftIO $ do
            tc <- atomically $ dupTChan inp
            newBot tc outp s b
        return a

newBot :: InChan -> OutChan -> s -> Bot s a -> IO ()
newBot inp outp s b = void . forkIO . void $
    S.runStateT (R.runReaderT (interpret b) (inp, outp)) s

