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
import Control.Monad.Free.Church
import Control.Monad.State
import Data.Either

import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.RWS

type InChan = TChan Message
type OutChan = TChan Response
type TimeoutChan = TChan (Maybe Channel, Int)

-- replace with something better
startTimeoutThread :: InChan -> IO TimeoutChan
startTimeoutThread inc = do
    ch <- newTChanIO
    forkIO $ forever $ do
        (chan, d) <- atomically $ readTChan ch
        forkIO $ do
            threadDelay d
            atomically $ writeTChan inc (Timeout chan)
    return ch

startBot :: s -> Bot s Message Response () -> IO (InChan, OutChan)
startBot initial bot = do
    inc <- newTChanIO
    outc <- newTChanIO
    toc <- startTimeoutThread inc
    let b = interpret bot
    forkIO $ flip evalStateT initial $ forever $ do
        s <- get 
        msg <- liftIO . atomically $ readTChan inc
        let (_, s', o) = runRWSBot b msg s
            (touts, resps) = partitionEithers o
        liftIO $ atomically $ do
            mapM_ (writeTChan toc) touts
            mapM_ (writeTChan outc) resps
        put s'
    atomically $ writeTChan inc (Timeout Nothing)
    return (inc, outc)

