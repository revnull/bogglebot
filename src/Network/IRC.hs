{-# LANGUAGE OverloadedStrings #-}

module Network.IRC (
                    Message(..)
                   ,msgChannel
                   ,isTimeout
                   ,isPing
                   ,User(..)
                   ,Channel
                   ,Response(..)
                   ,setChannel
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

type Channel = BS.ByteString

data Message = 
    Ping BS.ByteString
  | PrivMsg User Channel BS.ByteString
  | Join User Channel
  | Quit User Channel
  | Timeout (Maybe Channel)
  | Unknown BS.ByteString
  deriving (Read, Show, Eq, Ord)

msgChannel :: Message -> Maybe BS.ByteString
msgChannel (PrivMsg _ ch _) = Just ch
msgChannel (Join _ ch) = Just ch
msgChannel (Quit _ ch) = Just ch
msgChannel (Timeout mch) = mch
msgChannel _ = Nothing

isTimeout :: Message -> Bool
isTimeout (Timeout _) = True
isTimeout _ = False

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
  | SendTimeout Int (Maybe Channel)
  deriving (Read, Show, Eq, Ord)

setChannel :: Channel -> Response -> Response
setChannel ch (SendMsg _ a) = SendMsg ch a
setChannel ch (SendTimeout i _) = SendTimeout i (Just ch)
setChannel _ r = r

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

