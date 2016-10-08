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
import qualified Data.Sequence as Seq

import Network.IRC
import Network.IRC.Bot
import Network.IRC.Bot.RWS

type InChan = TChan Message
type OutChan = TChan Response
type TimeoutChan = TChan SystemMessage

-- replace with something better
startTimeoutThread :: InChan -> IO TimeoutChan
startTimeoutThread inc = do
    ch <- newTChanIO
    forkIO $ forever $ do
        (SetTimeout chan d) <- atomically $ readTChan ch
        forkIO $ do
            threadDelay d
            atomically $ writeTChan inc (Timeout chan)
    return ch

startBot :: Bot Message Response () -> IO (InChan, OutChan)
startBot bot = do
    inc <- newTChanIO
    outc <- newTChanIO
    toc <- startTimeoutThread inc
    forkIO $ void $ flip evalStateT (initialBot bot) $ forever $ do
        bots <- get 
        msg <- liftIO . atomically $ readTChan inc
        liftIO $ print msg
        let (bots', sys, outs) = stepBots msg bots
        liftIO $ atomically $ do
            mapM_ (writeTChan toc) sys
            mapM_ (writeTChan outc) outs
        liftIO $ print outs
        liftIO $ print (length sys)
        liftIO $ print (Seq.length bots')
        put bots'
    atomically $ writeTChan inc (Timeout Nothing)
    return (inc, outc)

