{-# LANGUAGE OverloadedStrings #-}

module Network.IRC (
                    Message(..)
                   ,User(..)
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

data User = User BS.ByteString BS.ByteString
  deriving (Read, Show, Eq, Ord)

data Message = 
    Ping BS.ByteString
  | PrivMsg User BS.ByteString BS.ByteString
  | Join User BS.ByteString
  | Quit User BS.ByteString
  | Unknown BS.ByteString
  deriving (Read, Show, Eq, Ord)

data Response =
    Pong BS.ByteString
  | Nick BS.ByteString
  | Login BS.ByteString
  | SendMsg BS.ByteString BS.ByteString
  | JoinChannel BS.ByteString
  | QuitChannel BS.ByteString
  deriving (Read, Show, Eq, Ord)

parseMessage :: Parser Message
parseMessage = (message <|> unknown) <* word8 13 <* word8 10 where
    message = ping <|> (user <*> (privmsg <|> join <|> quit))
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
    user = do 
        usr <- User <$>
            (word8 58 *> takeWhile1 (noneOf [13,32,33]) <* word8 33) <*>
            (takeWhile notWS)
        return ($ usr)
    unknown = Unknown <$> takeWhile (/= 13)
    noneOf l = not . flip elem l
    notWS = noneOf [32, 13]

encodeResponse :: Response -> BSL.ByteString
encodeResponse (Pong msg) = "PONG " <> BSL.fromStrict msg
encodeResponse (Nick n) = "NICK " <> BSL.fromStrict n
encodeResponse (Login u) = "USER " <> BSL.fromStrict u <> " 0 * :Boggle Bot"
encodeResponse (SendMsg ch msg) = "PRIVMSG " <> BSL.fromStrict ch <> " :" <>
    BSL.fromStrict msg
encodeResponse (JoinChannel ch) = "JOIN " <> BSL.fromStrict ch
encodeResponse (QuitChannel ch) = "QUIT " <> BSL.fromStrict ch

