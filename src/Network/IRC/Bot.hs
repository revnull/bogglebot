{-# LANGUAGE DeriveFunctor, ExistentialQuantification, Rank2Types #-}

module Network.IRC.Bot (
                        BotF(..)
                       ,Bot
                       ,RawBot
                       ,IRCBot
                       ,BotMessageF(..)
                       ,BotMessage
                       ,isTimeout
                       ,BotResponseF(..)
                       ,BotResponse
                       ,isFork
                       ,isSetTimeout
                       ,readIn
                       ,readIn'
                       ,waitFor
                       ,waitFor'
                       ,writeOut
                       ,writeOut'
                       ,timeout
                       ,channelTimeout
                       ,fork
                       ,readQuery
                       ,writeQuery
                       ,writeExceptQuery
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

import Control.Monad
import Control.Monad.Free.Church
import qualified Data.ByteString as BS
import Network.IRC
import Data.FixFile
import Control.Monad.Except
import Unsafe.Coerce

data BotMessageF a =
    Init
  | Timeout (Maybe Channel)
  | IRCMessage a
  deriving (Functor, Read, Show, Eq, Ord)

type BotMessage = BotMessageF Message

ircMessage :: BotMessageF a -> Maybe a
ircMessage (IRCMessage m) = Just m
ircMessage _ = Nothing

isTimeout :: BotMessageF a -> Bool
isTimeout (Timeout _) = True
isTimeout _ = False

data BotResponseF r a =
    SetTimeout (Maybe Channel) Int
  | Fork (IRCBot r ())
  | IRCResponse a
  deriving (Functor)

type BotResponse r = BotResponseF r Response

isFork :: BotResponseF r a -> Bool
isFork (Fork _) = True
isFork _ = False

isSetTimeout :: BotResponseF r a -> Bool
isSetTimeout (SetTimeout _ _) = True
isSetTimeout _ = False

data BotF r i o a =
    ReadIn (i -> Maybe a)
  | WriteOut o a
  | forall b. Query (FixFile r -> IO b) (b -> a)

instance Functor (BotF r i o) where
    fmap f (ReadIn g) = ReadIn (fmap f . g)
    fmap f (WriteOut o a) = WriteOut o (f a)
    fmap f (Query q g) = Query q (f . g)

type RawBot r i o = F (BotF r i o)

type Bot r i o = F (BotF r (BotMessageF i) (BotResponseF r o))

type IRCBot r = Bot r Message Response

readIn' :: RawBot r i o i
readIn' = liftF $ ReadIn Just

readIn :: Bot r i o i
readIn = liftF $ ReadIn ircMessage

waitFor' :: (i -> Maybe a) -> RawBot r i o a
waitFor' m = liftF $ ReadIn m

waitFor :: (i -> Maybe a) -> Bot r i o a
waitFor m = liftF $ ReadIn $ \msg -> case msg of
    IRCMessage i -> m i
    _ -> Nothing

writeOut' :: o -> RawBot r i o ()
writeOut' o = liftF $ WriteOut o ()

writeOut :: o -> Bot r i o ()
writeOut = writeOut' . IRCResponse

fork :: IRCBot r () -> Bot r i o ()
fork = writeOut' . Fork

timeout :: Int -> Bot r i o ()
timeout = writeOut' . SetTimeout Nothing

channelTimeout :: Channel -> Int -> Bot r i o ()
channelTimeout ch = writeOut' . SetTimeout (Just ch)

subBot :: (i -> i') -> (o' -> o) -> Bot r i' o' a -> Bot r i o a
subBot im om = hoistF phi where
    phi (ReadIn f) = ReadIn (f . fmap im)
    phi (WriteOut o a) = WriteOut (fmap om o) a
    phi q = unsafeCoerce q

filterBot' :: (BotMessageF i -> Bool) -> Bot r i o a -> Bot r i o a
filterBot' filt = hoistF phi where
    phi (ReadIn f) = ReadIn f' where
        f' i = guard (filt i) >> f i
    phi x = x

readQuery :: Root r => (forall s. Transaction r s a) -> Bot r i o a
readQuery t = liftF $ Query (\ff -> readTransaction ff t) id

writeQuery :: Root r => (forall s. Transaction r s a) -> Bot r i o a
writeQuery t = liftF $ Query (\ff -> writeTransaction ff t) id

writeExceptQuery :: Root r => (forall s. ExceptT e (Transaction r s) a) ->
    Bot r i o (Either e a)
writeExceptQuery t = liftF $ Query (\ff -> writeExceptTransaction ff t) id

handleChannel :: Channel -> Bot r Message BS.ByteString () ->
    Bot r Message Response ()
handleChannel ch b = filterBot' matchChan $ subBot id (SendMsg ch) b where
    matchChan (Timeout ch') = ch' == mch
    matchChan (IRCMessage msg) = msgChannel msg == mch
    matchChan _ = True
    mch = Just ch

handleJoin' :: Bot r (Channel, User) o () -> Bot r Message o ()
handleJoin' b = do
    r <- readIn
    case r of
        Join u ch -> subBot (const (ch, u)) id b
        _ -> return ()

handleJoin :: Bot r User o () -> Bot r Message o ()
handleJoin = handleJoin' . subBot snd id

handleQuit' :: Bot r (Channel, User) o () -> Bot r Message o ()
handleQuit' b = do
    r <- readIn
    case r of
        Quit u ch -> subBot (const (ch, u)) id b
        _ -> return ()

handleQuit :: Bot r User o () -> Bot r Message o ()
handleQuit = handleQuit' . subBot snd id

handlePrivMsg' :: Bot r (Channel, (User, BS.ByteString)) o () ->
    Bot r Message o ()
handlePrivMsg' b = do
    r <- readIn
    case r of
        PrivMsg u ch v -> subBot (const (ch, (u, v))) id b
        _ -> return ()

handlePrivMsg :: Bot r (User, BS.ByteString) o () -> Bot r Message o ()
handlePrivMsg = handlePrivMsg' . subBot snd id

handleTimeout :: Bot r (Maybe Channel) o () -> Bot r Message o ()
handleTimeout b = do
    r <- readIn'
    case r of
        Timeout mch -> subBot (const mch) id b
        _ -> return ()

pingBot :: IRCBot r ()
pingBot = forever $ do
    Ping msg <- waitFor (\i -> guard (isPing i) >> return i)
    writeOut (Pong msg)

echoBot :: Channel -> IRCBot r ()
echoBot ch = handleChannel ch . handlePrivMsg $ do
    (_, msg) <- readIn
    writeOut msg

