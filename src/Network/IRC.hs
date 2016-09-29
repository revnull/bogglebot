{-# LANGUAGE OverloadedStrings #-}

module Network.IRC (
                    Message(..)
                   ,Response(..)
                   ,parseMessage
                   ,encodeResponse
                   ) where

import Control.Applicative
import Control.Monad hiding (join)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Prelude hiding (takeWhile)

data Message = 
    Ping BS.ByteString
  | PrivMsg BS.ByteString BS.ByteString BS.ByteString
  | Join BS.ByteString BS.ByteString
  | Quit BS.ByteString BS.ByteString
  | Unknown BS.ByteString
  deriving (Read, Show, Eq, Ord)

data Response =
    Pong BS.ByteString
  | Nick BS.ByteString
  | User BS.ByteString
  | SendMsg BS.ByteString BS.ByteString
  | JoinChannel BS.ByteString
  | QuitChannel BS.ByteString

parseMessage :: Parser Message
parseMessage = (message <|> unknown) <* word8 13 <* word8 10 where
    message = ping <|> (channel <*> (privmsg <|> join <|> quit))
    ping = string "PING " *> (Ping <$> takeWhile1 (/= 13))
    channel = do
        ch <- takeWhile1 notWS
        return ($ ch)
    privmsg = do
        usr <- string " PRIVMSG " *> takeWhile1 notWS
        void $ string " :"
        msg <- takeWhile1 (/= 13)
        return $ \ch -> PrivMsg ch usr msg
    join = do
        usr <- string " JOIN :" *> takeWhile1 (/= 13)
        return $ \ch -> Join ch usr
    quit = do
        usr <- string " QUIT :" *> takeWhile1 (/= 13)
        return $ \ch -> Quit ch usr
    unknown = Unknown <$> takeWhile (/= 13)
    notWS c = c /= 32 && c /= 13

encodeResponse :: Response -> BSL.ByteString
encodeResponse (Pong msg) = "PONG " <> BSL.fromStrict msg
encodeResponse (Nick n) = "NICK " <> BSL.fromStrict n
encodeResponse (User u) = "USER " <> BSL.fromStrict u <> " 0 * :Boggle Bot"
encodeResponse (SendMsg ch msg) = "PRIVMSG " <> BSL.fromStrict ch <> " " <>
    BSL.fromStrict msg
encodeResponse (JoinChannel ch) = "JOIN " <> BSL.fromStrict ch
encodeResponse (QuitChannel ch) = "QUIT " <> BSL.fromStrict ch

