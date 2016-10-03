{-# LANGUAGE OverloadedStrings, ExistentialQuantification,
    FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Network.IRC.Bot (
                        BotF(..)
                       ,Bot(..)
                       ,await
                       ,respond
                       ,timeout
                       ,fork
                       ,runChannel
                       ,waitFor
                       ,pingBot
                       ,echoBot
                       ) where

import Control.Applicative
import Control.Monad
import Data.ByteString as BS
import Control.Monad.State.Class
import Network.IRC
import Control.Monad.Free.Church

data BotF s a = 
    Await (Maybe Message -> a)
  | Respond Response a
  | Timeout Int a
  | Get (s -> a)
  | Put s a
  | forall s' . Fork s' (Bot s' ()) a

instance Functor (BotF s) where
    fmap f (Await g) = Await (f . g)
    fmap f (Respond r a) = Respond r (f a)
    fmap f (Timeout i a) = Timeout i (f a)
    fmap f (Get g) = Get (f . g)
    fmap f (Put s a) = Put s (f a)
    fmap f (Fork s b a) = Fork s b (f a)

newtype Bot s a = Bot { runBot :: F (BotF s) a }

instance Functor (Bot s) where
    fmap f (Bot b) = Bot (fmap f b)

instance Applicative (Bot s) where
    pure a = Bot (pure a)
    Bot a <*> Bot b = Bot (a <*> b)

instance Monad (Bot s) where
    return = pure
    Bot b >>= f = Bot $ do
        a <- b
        let Bot c = f a
        c

instance MonadState s (Bot s) where
    get = Bot $ liftF $ Get id
    put s = Bot $ liftF $ Put s ()

await :: Bot s (Maybe Message)
await = Bot $ liftF $ Await id

respond :: Response -> Bot s ()
respond resp = Bot $ liftF $ Respond resp ()

timeout :: Int -> Bot s ()
timeout t = Bot $ liftF $ Timeout t ()

fork :: s' -> Bot s' a -> Bot s ()
fork s bot = Bot $ liftF $ Fork s (void bot) ()

waitFor :: (Maybe Message -> Bool) -> Bot s (Maybe Message)
waitFor filt = wf where
    wf = do
        m <- await
        if filt m then return m else wf

runChannel :: BS.ByteString -> Bot s a -> Bot s ()
runChannel ch (Bot bot) = run where
    run = do
        respond (JoinChannel ch)
        foldF chHandler (void bot)
    mch = Just ch
    filt :: Maybe Message -> Bool
    filt Nothing = True
    filt (Just m) = msgChannel m == mch
    chHandler (Await f) = f <$> waitFor filt
    chHandler (Respond r a) = respond r >> return a
    chHandler (Timeout t a) = timeout t >> return a
    chHandler (Get f) = f <$> get
    chHandler (Put s a) = put s >> return a
    chHandler (Fork s b a) = fork s b >> return a

pingBot :: Bot () ()
pingBot = forever pb where
    isPing (Just (Ping _)) = True
    isPing _ = False
    pb = do
        Just (Ping msg) <- waitFor isPing
        respond (Pong msg)

echoBot :: BS.ByteString -> Bot () ()
echoBot ch = eb where
    isPrivMsg (Just (PrivMsg _ _ _)) = True
    isPrivMsg _ = False
    eb = runChannel ch $ forever $ do
        (Just (PrivMsg _ _ msg)) <- waitFor isPrivMsg
        respond (SendMsg ch msg)
