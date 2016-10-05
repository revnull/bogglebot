{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeSynonymInstances,
    FlexibleInstances, MultiParamTypeClasses #-}

module Network.IRC.Bot (
                        BotF(..)
                       ,Bot(..)
                       ,readIn
                       ,writeOut
                       ,timeout
                       ,channelTimeout
                       ,botState
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

data BotF s i o a =
    ReadIn (i -> a)
  | WriteOut o a
  | GetState (s -> a)
  | PutState s a
  | MapState (s -> s) a
  | PutTimeout (Maybe Channel) Int a

instance Functor (BotF s i o) where
    fmap f (ReadIn g) = ReadIn (f . g)
    fmap f (WriteOut o a) = WriteOut o (f a)
    fmap f (GetState g) = GetState (f . g)
    fmap f (PutState s a) = PutState s (f a)
    fmap f (MapState g a) = MapState g (f a)
    fmap f (PutTimeout mch i a) = PutTimeout mch i (f a)

newtype Bot s i o a = Bot { runBot :: F (BotF s i o) a }

instance Functor (Bot s i o) where
    fmap f (Bot b) = Bot $ fmap f b

instance Applicative (Bot s i o) where
    pure = Bot . return
    Bot ab <*> Bot a = Bot $ ab <*> a

instance Monad (Bot s i o) where
    return = pure
    Bot a >>= f = Bot $ do
        a' <- a
        runBot (f a')

instance MonadState s (Bot s i o) where
    get = Bot . liftF $ GetState id
    put s = Bot . liftF $ PutState s ()

constLens :: b -> Lens a a b c
constLens b = lens (const b) const

unitLens :: Lens a a () b
unitLens = constLens ()

_id :: Lens' a a
_id f a = fmap id (f a)

readIn :: Bot s i o i
readIn = Bot . liftF $ ReadIn id

writeOut :: o -> Bot s i o ()
writeOut o = Bot . liftF $ WriteOut o ()

timeout :: Int -> Bot s i o ()
timeout i = Bot $ liftF $ PutTimeout Nothing i ()

channelTimeout :: Channel -> Int -> Bot s i o ()
channelTimeout ch i = Bot $ liftF $ PutTimeout (Just ch) i ()

subBot :: Lens' s s' -> (i -> i') -> (o' -> o) -> Bot s' i' o' a -> Bot s i o a
subBot l im om = Bot . hoistF phi . runBot where
    phi (ReadIn f) = ReadIn (f . im)
    phi (WriteOut o a) = WriteOut (om o) a
    phi (GetState f) = GetState (f . (^.l))
    phi (PutState s a) = MapState (set l s) a
    phi (MapState f a) = MapState setLens a where
        setLens s = set l (f $ s^.l) s
    phi (PutTimeout mch i a) = PutTimeout mch i a

botState :: Lens' s s' -> Bot s' i o a -> Bot s i o a
botState l = subBot l id id

handleChannel :: Channel -> Bot s Message BS.ByteString () ->
    Bot s Message Response ()
handleChannel ch b = do
    r <- readIn
    when (msgChannel r == Just ch) $
        subBot _id id (SendMsg ch) b

handleJoin' :: Bot s (Channel, User) o () -> Bot s Message o ()
handleJoin' b = do
    r <- readIn
    case r of
        Join u ch -> subBot _id (const (ch, u)) id b
        _ -> return ()

handleJoin :: Bot s User o () -> Bot s Message o ()
handleJoin = handleJoin' . subBot _id snd id

handleQuit' :: Bot s (Channel, User) o () -> Bot s Message o ()
handleQuit' b = do
    r <- readIn
    case r of
        Quit u ch -> subBot _id (const (ch, u)) id b
        _ -> return ()

handleQuit :: Bot s User o () -> Bot s Message o ()
handleQuit = handleQuit' . subBot _id snd id

handlePrivMsg' :: Bot s (Channel, (User, BS.ByteString)) o () ->
    Bot s Message o ()
handlePrivMsg' b = do
    r <- readIn
    case r of
        PrivMsg u ch v -> subBot _id (const (ch, (u, v))) id b
        _ -> return ()

handlePrivMsg :: Bot s (User, BS.ByteString) o () -> Bot s Message o ()
handlePrivMsg = handlePrivMsg' . subBot _id snd id

handleTimeout :: Bot s (Maybe Channel) o () -> Bot s Message o ()
handleTimeout b = do
    r <- readIn
    case r of
        Timeout mch -> subBot _id (const mch) id b
        _ -> return ()

pingBot :: Bot s Message Response ()
pingBot = do
    r <- readIn
    case r of
        Ping msg -> writeOut (Pong msg)
        _ -> return ()

echoBot :: Channel -> Bot s Message Response ()
echoBot ch = handleChannel ch . handlePrivMsg $ do
    (_, msg) <- readIn
    writeOut msg

