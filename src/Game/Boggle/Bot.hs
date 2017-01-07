{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Game.Boggle.Bot (
                        boggleBot
                       ,sanitize
                       ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Foldable hiding (forM_, mapM_)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.String
import Data.FixFile
import qualified Data.FixFile.Trie.Light as T
import System.Random

import Game.Boggle
import Network.IRC
import Network.IRC.Bot

chunkWords :: [BSL.ByteString] -> [[BSL.ByteString]]
chunkWords = ($ []) . chunk' 0 id where
    chunk' 0 _ [] = id
    chunk' _ f [] = (f []:)
    chunk' c f l@(x:xs)
        | c > 100 = (f []:) . chunk' 0 id l
        | otherwise = chunk' (c + BSL.length x) (f . (x:)) xs

cap :: BS.ByteString -> BS.ByteString
cap = BS.map capW8 where
    capW8 c
        | c >= 97 && c <= 122 = c - 32
        | otherwise = c

capPM :: Message -> Message
capPM (PrivMsg usr ch msg) = PrivMsg usr ch (cap msg)
capPM x = x

sanitize :: BSL.ByteString -> Maybe BSL.ByteString
sanitize bs = sanitized where
    sanitized = do
        guard (BSL.length bs >= 3)
        BSL.pack <$> traverse san (BSL.unpack bs)
    san x
      | x >= 65 && x <= 90 = return x
      | x >= 97 && x <= 122 = return (x - 32)
      | otherwise = Nothing

boggleBot :: Channel -> StdGen -> IRCBot (Ref Trie) ()
boggleBot ch g = do
    handleChannel ch $ flip evalStateT (20 :: Int, (g, Nothing)) $ forever $ do
        msg <- lift $ fmap capPM <$> readIn'
        r <- zoom _2 $ gameRunning
        warned <- zoom _2 $ hasWarned
        case (msg, r, warned) of

            (IRCMessage (PrivMsg _ _ "BOGGLE TIME"), False, _) -> do
                _1 .= 20
                void $ zoom _2 $ (lift (readQuery getFull) >>= newGame)
                lift $ channelTimeout ch 120000000
                Just (b, ws, _, _) <- use (_2._2)
                let ms = getSum $ foldMap (Sum . wordValue) ws
                lift $ do
                    writeOut "It's Boggle Time!"
                    writeOut ("Maximum Score " <> fromString (show ms))
                    forM_ (boardLines b) $ \l -> do
                        writeOut (BSL.toStrict l)

            (IRCMessage (PrivMsg _ _ "HELP"), False, _) -> lift $ do
                writeOut "BoggleBot Commands:"
                writeOut "\"boggle time\" - starts a game of boggle"
                writeOut "\"!board\" - displays the current board"
                writeOut "\"help\" - displays this help message."
                writeOut "\"lookup $WORD\" - lookup a word in the dictionary."
                writeOut "\"insert $WORD\" - insert a word into the dictionary."
                writeOut "\"delete $WORD\" - delete a word from the dictionary."

            (IRCMessage (PrivMsg _ _ cmd), False, _) -> lift $ do
                let cmds = BSL.split 32 $ BSL.fromStrict cmd
                case catMaybes $ fmap sanitize cmds of
                    "LOOKUP":ws -> forM_ ws $ \w -> do
                        unless (BSL.null w) $ do
                            lu <- readQuery (T.lookupTrieT w)
                            if isJust lu
                                then writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" is a word")
                                else writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" is not a word")
                    "INSERT":ws -> forM_ ws $ \w -> do
                        unless (BSL.null w) $ do
                            ins <- writeExceptQuery $ do
                                lu <- lift $ T.lookupTrieT w
                                if isJust lu
                                    then throwError ()
                                    else lift $ T.insertTrieT w ()
                            case ins of
                                Left _ -> writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" was already present")
                                Right _ -> writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" has been added")
                    "DELETE":ws -> forM_ ws $ \w -> do
                        unless (BSL.null w) $ do
                            del <- writeExceptQuery $ do
                                lu <- lift $ T.lookupTrieT w
                                if isJust lu
                                    then lift $ T.deleteTrieT w
                                    else throwError ()
                            case del of
                                Left _ -> writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" was not present")
                                Right _ -> writeOut . BSL.toStrict $
                                    ("\"" <> w <> "\" has been deleted")
                    _ -> return ()

            (_, False, _) -> return ()

            (Timeout _, _, True) -> do
                Just (scores, missed) <- zoom _2 $ endGame
                lift $ writeOut "Time's up!"
                let scores' = L.reverse $ sortBy (compare `on` (^._3)) scores
                lift $ forM_ scores' $ \(p, ws, s) -> do
                    writeOut . BSL.toStrict $
                        (p <> " : " <> fromString (show s))
                    forM_ (chunkWords ws) $ \ws' -> writeOut . BSL.toStrict $
                        (p <> " : " <> BSL.intercalate ", " ws')
                lift $ forM_ (chunkWords missed) $ \miss -> do
                    writeOut . BSL.toStrict $
                        ("Missed Words: " <> BSL.intercalate ", " miss)

            (Timeout _, _, False) -> do
                zoom _2 $ warn
                lift $ writeOut "One minute remaining!"
                lift $ channelTimeout ch 60000000

            (IRCMessage (PrivMsg (User usr _) _ str), _, _) -> do
                _1 -= 1
                lc <- use _1
                when (lc == 0) $ do
                    _1 .= 20
                    Just (b, _, _, _) <- use (_2._2)
                    lift $ forM_ (boardLines b) $ \l -> do
                        writeOut (BSL.toStrict l)

                let bs = catMaybes . fmap sanitize . BSL.split 32 $ str'
                    str' = BSL.fromStrict str
                    usr' = BSL.fromStrict usr
                zoom _2 $ mapM_ (scoreWord usr') bs

                when (str == "!BOARD") $ do
                    Just (b, _, _, _) <- use (_2._2)
                    lift $ forM_ (boardLines b) $ \l -> do
                        writeOut (BSL.toStrict l)

            _ -> return ()
