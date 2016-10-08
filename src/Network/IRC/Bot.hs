{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeSynonymInstances,
    FlexibleInstances, MultiParamTypeClasses #-}

module Network.IRC.Bot (
                        BotF(..)
                       ,Bot(..)
                       ,SystemMessage(..)
                       ,readIn
                       ,waitFor
                       ,writeOut
                       ,timeout
                       ,channelTimeout
                       ,fork
                       ,handleChannel
                       ,handleJoin'
                       ,handleJoin
                       ,handleQuit'
                       ,handleQuit
                       ,handlePrivMsg'
                       ,handlePrivMsg
                       ,handleTimeout
                       ,pingBot
                       ,echoBot
                       ) where

import Control.Applicative 
import Control.Lens
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.State.Class
import qualified Data.ByteString as BS
import Network.IRC

data SystemMessage =
    SetTimeout (Maybe Channel) Int
  | Fork (Bot Message Response ())

data BotF i o a =
    ReadIn (i -> Bool) (i -> a)
  | WriteOut o a
  | System SystemMessage a

type Bot i o = F (BotF i o)

instance Functor (BotF i o) where
    fmap f (ReadIn p g) = ReadIn p (f . g)
    fmap f (WriteOut o a) = WriteOut o (f a)
    fmap f (System r a) = System r (f a)

readIn :: Bot i o i
readIn = liftF $ ReadIn (const True) id

waitFor :: (i -> Bool) -> Bot i o i
waitFor pred = liftF $ ReadIn pred id

writeOut :: o -> Bot i o ()
writeOut o = liftF $ WriteOut o ()

system :: SystemMessage -> Bot i o ()
system sm = liftF $ System sm ()

fork :: Bot Message Response () -> Bot i o ()
fork = system . Fork

timeout :: Int -> Bot i o ()
timeout = system . SetTimeout Nothing

channelTimeout :: Channel -> Int -> Bot i o ()
channelTimeout ch = system . SetTimeout (Just ch)

subBot :: (i -> i') -> (o' -> o) -> Bot i' o' a -> Bot i o a
subBot im om = hoistF phi where
    phi (ReadIn p f) = ReadIn (p . im) (f . im)
    phi (WriteOut o a) = WriteOut (om o) a
    phi (System sm a) = System sm a

filterBot :: (i -> Bool) -> Bot i o a -> Bot i o a
filterBot filt = hoistF phi where
    phi (ReadIn p f) = ReadIn (\i -> filt i && p i) f
    phi x = x

handleChannel :: Channel -> Bot Message BS.ByteString () ->
    Bot Message Response ()
handleChannel ch b = filterBot ((Just ch ==) . msgChannel) $
    subBot id (SendMsg ch) b

handleJoin' :: Bot (Channel, User) o () -> Bot Message o ()
handleJoin' b = do
    r <- readIn
    case r of
        Join u ch -> subBot (const (ch, u)) id b
        _ -> return ()

handleJoin :: Bot User o () -> Bot Message o ()
handleJoin = handleJoin' . subBot snd id

handleQuit' :: Bot (Channel, User) o () -> Bot Message o ()
handleQuit' b = do
    r <- readIn
    case r of
        Quit u ch -> subBot (const (ch, u)) id b
        _ -> return ()

handleQuit :: Bot User o () -> Bot Message o ()
handleQuit = handleQuit' . subBot snd id

handlePrivMsg' :: Bot (Channel, (User, BS.ByteString)) o () ->
    Bot Message o ()
handlePrivMsg' b = do
    r <- readIn
    case r of
        PrivMsg u ch v -> subBot (const (ch, (u, v))) id b
        _ -> return ()

handlePrivMsg :: Bot (User, BS.ByteString) o () -> Bot Message o ()
handlePrivMsg = handlePrivMsg' . subBot snd id

handleTimeout :: Bot (Maybe Channel) o () -> Bot Message o ()
handleTimeout b = do
    r <- readIn
    case r of
        Timeout mch -> subBot (const mch) id b
        _ -> return ()

pingBot :: Bot Message Response ()
pingBot = forever $ do
    Ping msg <- waitFor isPing
    writeOut (Pong msg)

echoBot :: Channel -> Bot Message Response ()
echoBot ch = handleChannel ch . handlePrivMsg $ do
    (_, msg) <- readIn
    writeOut msg

