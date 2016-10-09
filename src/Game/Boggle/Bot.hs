{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Game.Boggle.Bot (
                        boggleBot
                       ) where

import Control.Applicative
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

boggleBot :: Trie -> Channel -> StdGen -> Bot Message Response ()
boggleBot t ch g = do
    handleChannel ch $ flip evalStateT (g, Nothing) $ forever $ do
        msg <- lift $ capPM <$> readIn
        r <- gameRunning
        w <- hasWarned
        case (msg, r, w) of
            (PrivMsg _ _ "BOGGLE TIME", False, _) -> do
                void $ newGame t
                lift $ channelTimeout ch 120000000
                (_, Just (b, ws, _, _)) <- get
                let ms = getSum $ foldMap (Sum . wordValue) ws
                lift $ do
                    writeOut "It's Boggle Time!"
                    writeOut ("Maximum Score " <> fromString (show ms))
                    forM_ (boardLines b) $ \l -> do
                        writeOut l
            (_, False, _) -> return ()
            (Timeout _, _, True) -> do
                Just (scores, missed) <- endGame
                lift $ writeOut "Time's up!"
                let scores' = L.reverse $ sortBy (compare `on` snd) scores
                forM_ scores' $ \(p, s) -> do
                    lift $ writeOut (p <> " : " <> fromString (show s))
                forM_ (chunkWords missed) $ \miss -> do
                    lift $ writeOut ("Missed Words: " <> BS.intercalate ", " miss)
            (Timeout _, _, False) -> do
                warn
                lift $ writeOut "One minute remaining!"
                lift $ channelTimeout ch 60000000
            (PrivMsg (User usr _) _ str, _, _) -> do
                let bs = catMaybes . fmap sanitize . BS.split 32 $ str
                mapM_ (scoreWord usr) bs

                when (str == "!BOARD") $ do
                    (_, Just (b, _, _, _)) <- get
                    lift $ forM_ (boardLines b) $ \l -> do
                        writeOut l
            _ -> return ()
