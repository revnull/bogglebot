{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Bot.STM (
                            startTimeoutThread
                           ,startBot
                           ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.RWS

type InChan = TChan Message
type BotInChan = TChan BotMessage
type OutChan = TChan Response
type TimeoutChan = TChan (Maybe Channel, Int)

-- replace with something better
startTimeoutThread :: BotInChan -> IO TimeoutChan
startTimeoutThread inc = do
    ch <- newTChanIO
    void $ forkIO $ forever $ do
        (chan, d) <- atomically $ readTChan ch
        forkIO $ do
            threadDelay d
            atomically $ writeTChan inc (Timeout chan)
    return ch

startBot :: IRCBot () -> IO (InChan, OutChan)
startBot bot = do
    inc <- newTChanIO
    bmin <- newTChanIO
    outc <- newTChanIO
    toc <- startTimeoutThread bmin
    void $ forkIO $ void $ flip evalStateT (initialBot bot) $ forever $ do
        bots <- get 
        msg <- liftIO . atomically $ do
            empt <- isEmptyTChan inc
            if empt then readTChan bmin else IRCMessage <$> readTChan inc
        let (bots', outs) = stepBots msg bots
        liftIO $ atomically $ do
            forM_ outs $ \out -> case out of
                IRCResponse r -> writeTChan outc r
                SetTimeout ch i -> writeTChan toc (ch, i)
                _ -> error "Unhandled BotResponse"
        put bots'
    atomically $ writeTChan bmin Init
    return (inc, outc)

