{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Game.Boggle.Bot (
                        boggleBot
                       ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Foldable hiding (forM_, mapM_)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.String

import Data.Trie
import Game.Boggle
import Network.IRC
import Network.IRC.Bot
import System.Random

chunkWords :: [BS.ByteString] -> [[BS.ByteString]]
chunkWords = ($ []) . chunk' 0 id where
    chunk' 0 _ [] = id
    chunk' _ f [] = (f []:)
    chunk' c f l@(x:xs)
        | c > 100 = (f []:) . chunk' 0 id l
        | otherwise = chunk' (c + BS.length x) (f . (x:)) xs

cap :: BS.ByteString -> BS.ByteString
cap = BS.map capW8 where
    capW8 c
        | c >= 97 && c <= 122 = c - 32
        | otherwise = c

capPM :: Message -> Message
capPM (PrivMsg usr ch msg) = PrivMsg usr ch (cap msg)
capPM x = x

boggleBot :: Trie -> Channel -> StdGen -> IRCBot ()
boggleBot t ch g = do
    handleChannel ch $ flip evalStateT (20 :: Int, (g, Nothing)) $ forever $ do
        msg <- lift $ fmap capPM <$> readIn'
        r <- zoom _2 $ gameRunning
        w <- zoom _2 $ hasWarned
        case (msg, r, w) of

            (IRCMessage (PrivMsg _ _ "BOGGLE TIME"), False, _) -> do
                _1 .= 20
                void $ zoom _2 $ newGame t
                lift $ channelTimeout ch 120000000
                Just (b, ws, _, _) <- use (_2._2)
                let ms = getSum $ foldMap (Sum . wordValue) ws
                lift $ do
                    writeOut "It's Boggle Time!"
                    writeOut ("Maximum Score " <> fromString (show ms))
                    forM_ (boardLines b) $ \l -> do
                        writeOut l

            (_, False, _) -> return ()

            (Timeout _, _, True) -> do
                Just (scores, missed) <- zoom _2 $ endGame
                lift $ writeOut "Time's up!"
                let scores' = L.reverse $ sortBy (compare `on` (^._3)) scores
                forM_ scores' $ \(p, ws, s) -> do
                    lift $ writeOut (p <> " : " <> fromString (show s))
                    forM_ (chunkWords ws) $ \ws' ->
                        lift $ writeOut (p <> " : " <> BS.intercalate ", " ws')
                forM_ (chunkWords missed) $ \miss -> do
                    lift $ writeOut
                        ("Missed Words: " <> BS.intercalate ", " miss)

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
                        writeOut l

                let bs = catMaybes . fmap sanitize . BS.split 32 $ str
                zoom _2 $ mapM_ (scoreWord usr) bs

                when (str == "!BOARD") $ do
                    Just (b, _, _, _) <- use (_2._2)
                    lift $ forM_ (boardLines b) $ \l -> do
                        writeOut l

            _ -> return ()
