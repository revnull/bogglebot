{-# LANGUAGE OverloadedStrings #-}

module Network.IRC (
                    Message(..)
                   ,channel
                   ,payload
                   ,user
                   ,commandNumber
                   ,msgChannel
                   ,isPing
                   ,User(..)
                   ,nick
                   ,fullName
                   ,Channel
                   ,Response(..)
                   ,setChannel
                   ,parseMessage
                   ,encodeResponse
                   ) where

import Control.Applicative
import Control.Lens hiding (noneOf)
import Control.Monad hiding (join)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Prelude hiding (takeWhile, take)

data User = User BS.ByteString BS.ByteString
  deriving (Read, Show, Eq, Ord)

nick :: Lens' User BS.ByteString
nick f (User ni fu) = (\ni' -> User ni' fu) <$> f ni

fullName :: Lens' User BS.ByteString
fullName f (User ni fu) = (User ni) <$> f fu

type Channel = BS.ByteString

data Message = 
    Ping BS.ByteString
  | PrivMsg User Channel BS.ByteString
  | Join User Channel
  | Quit User Channel
  | Command BS.ByteString Int BS.ByteString
  | Notice BS.ByteString BS.ByteString BS.ByteString
  | Unknown BS.ByteString
  deriving (Read, Show, Eq, Ord)

channel :: Traversal' Message BS.ByteString
channel f (PrivMsg u ch val) = PrivMsg u <$> f ch <*> pure val
channel f (Join u ch) = Join u <$> f ch
channel f (Quit u ch) = Quit u <$> f ch
channel _ x = pure x 

payload :: Traversal' Message BS.ByteString
payload f (Ping val) = Ping <$>  f val
payload f (PrivMsg u ch val) = PrivMsg u ch <$> f val
payload f (Command srv i val) = Command srv i <$> f val
payload f (Notice a b val) = Notice a b <$> f val
payload _ x = pure x

user :: Traversal' Message User
user f (PrivMsg usr ch val) = PrivMsg <$> f usr <*> pure ch <*> pure val
user f (Join usr ch) = Join <$> f usr <*> pure ch
user f (Quit usr ch) = Quit <$> f usr <*> pure ch
user _ x = pure x

commandNumber :: Traversal' Message Int
commandNumber f (Command srv i val) = Command srv <$> f i <*> pure val
commandNumber _ x = pure x

msgChannel :: Message -> Maybe BS.ByteString
msgChannel (PrivMsg _ ch _) = Just ch
msgChannel (Join _ ch) = Just ch
msgChannel (Quit _ ch) = Just ch
msgChannel _ = Nothing

isPing :: Message -> Bool
isPing (Ping _) = True
isPing _ = False

data Response =
    Pong BS.ByteString
  | Nick BS.ByteString
  | Login BS.ByteString
  | SendMsg Channel BS.ByteString
  | JoinChannel Channel
  | QuitChannel Channel
  deriving (Read, Show, Eq, Ord)

setChannel :: Channel -> Response -> Response
setChannel ch (SendMsg _ a) = SendMsg ch a
setChannel _ r = r

parseMessage :: Parser Message
parseMessage = (message <|> unknown) <* word8 13 <* word8 10 where
    message = ping <|>
        (fulluser <*> (privmsg <|> join <|> quit)) <|>
        command <|> notice
    ping = string "PING " *> (Ping <$> takeWhile1 (/= 13))
    privmsg = do
        ch <- string " PRIVMSG " *> takeWhile1 notWS
        void $ string " :"
        msg <- takeWhile1 (/= 13)
        return $ \usr -> PrivMsg usr ch msg
    join = do
        ch <- string " JOIN :" *> takeWhile1 (/= 13)
        return $ \usr -> Join usr ch
    quit = do
        ch <- string " QUIT :" *> takeWhile1 (/= 13)
        return $ \usr -> Quit usr ch
    fulluser = do 
        usr <- User <$>
            (word8 58 *> takeWhile1 (noneOf [13,32,33]) <* word8 33) <*>
            (takeWhile notWS)
        return ($ usr)
    command = Command <$> (word8 58 *> takeWhile1 notWS) <*>
        (word8 32 *> commNumber <* word8 32) <*>
        takeWhile (/= 13)
    notice = Notice <$> (word8 58 *> takeWhile1 notWS) <*>
        (string " NOTICE " *> takeWhile1 (/= 58) <* word8 58) <*>
        takeWhile (/= 13)
    unknown = Unknown <$> takeWhile (/= 13)
    noneOf l = not . flip elem l
    notWS = noneOf [32, 13]
    commNumber = toNum 100 <$> count 3 (satisfy isDigit)
    isDigit i = i >= 48 && i <= 57
    toNum _ [] = 0
    toNum i (x:xs) = (fromIntegral x - 48) * i + toNum (i `div` 10) xs

encodeResponse :: Response -> BSL.ByteString
encodeResponse (Pong msg) = "PONG " <> BSL.fromStrict msg
encodeResponse (Nick n) = "NICK " <> BSL.fromStrict n
encodeResponse (Login u) = "USER " <> BSL.fromStrict u <> " 0 * :Boggle Bot"
encodeResponse (SendMsg ch msg) = "PRIVMSG " <> BSL.fromStrict ch <> " :" <>
    BSL.fromStrict msg
encodeResponse (JoinChannel ch) = "JOIN " <> BSL.fromStrict ch
encodeResponse (QuitChannel ch) = "QUIT " <> BSL.fromStrict ch

