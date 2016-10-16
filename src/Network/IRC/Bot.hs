{-# LANGUAGE DeriveFunctor #-}

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

data BotResponseF a =
    SetTimeout (Maybe Channel) Int
  | Fork (IRCBot ())
  | IRCResponse a
  deriving (Functor)

type BotResponse = BotResponseF Response

isFork :: BotResponseF a -> Bool
isFork (Fork _) = True
isFork _ = False

isSetTimeout :: BotResponseF a -> Bool
isSetTimeout (SetTimeout _ _) = True
isSetTimeout _ = False

data BotF i o a =
    ReadIn (i -> Maybe a)
  | WriteOut o a
  deriving (Functor)

type RawBot i o = F (BotF i o)

type Bot i o = F (BotF (BotMessageF i) (BotResponseF o))

type IRCBot = Bot Message Response

readIn' :: RawBot i o i
readIn' = liftF $ ReadIn Just

readIn :: Bot i o i
readIn = liftF $ ReadIn ircMessage

waitFor' :: (i -> Maybe a) -> RawBot i o a
waitFor' m = liftF $ ReadIn m

waitFor :: (i -> Maybe a) -> Bot i o a
waitFor m = liftF $ ReadIn $ \msg -> case msg of
    IRCMessage i -> m i
    _ -> Nothing

writeOut' :: o -> RawBot i o ()
writeOut' o = liftF $ WriteOut o ()

writeOut :: o -> Bot i o ()
writeOut = writeOut' . IRCResponse

fork :: IRCBot () -> Bot i o ()
fork = writeOut' . Fork

timeout :: Int -> Bot i o ()
timeout = writeOut' . SetTimeout Nothing

channelTimeout :: Channel -> Int -> Bot i o ()
channelTimeout ch = writeOut' . SetTimeout (Just ch)

subBot :: (i -> i') -> (o' -> o) -> Bot i' o' a -> Bot i o a
subBot im om = hoistF phi where
    phi (ReadIn f) = ReadIn (f . fmap im)
    phi (WriteOut o a) = WriteOut (fmap om o) a

filterBot' :: (BotMessageF i -> Bool) -> Bot i o a -> Bot i o a
filterBot' filt = hoistF phi where
    phi (ReadIn f) = ReadIn f' where
        f' i = guard (filt i) >> f i
    phi x = x

handleChannel :: Channel -> Bot Message BS.ByteString () ->
    Bot Message Response ()
handleChannel ch b = filterBot' matchChan $ subBot id (SendMsg ch) b where
    matchChan (Timeout ch') = ch' == mch
    matchChan (IRCMessage msg) = msgChannel msg == mch
    matchChan _ = True
    mch = Just ch

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
    r <- readIn'
    case r of
        Timeout mch -> subBot (const mch) id b
        _ -> return ()

pingBot :: IRCBot ()
pingBot = forever $ do
    Ping msg <- waitFor (\i -> guard (isPing i) >> return i)
    writeOut (Pong msg)

echoBot :: Channel -> IRCBot ()
echoBot ch = handleChannel ch . handlePrivMsg $ do
    (_, msg) <- readIn
    writeOut msg

