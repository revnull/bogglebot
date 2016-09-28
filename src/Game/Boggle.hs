{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Game.Boggle(
                   randomBoard
                  ,solver
                   ) where

import Control.Monad
import Data.Monoid
import Data.Word
import Data.Array
import Data.Trie
import Data.Bits
import qualified Data.ByteString as BS
import System.Random
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe

type Freqs = M.Map BS.ByteString [Int]

letterFreqs :: Freqs
letterFreqs = M.fromList [
    ("QU", [3]),
    ("A", [73, 15, 2, 1, 1]),
    ("C", [43, 6, 1]),
    ("B", [21, 2, 1, 1]),
    ("E", [100, 35, 7, 1, 1]),
    ("D", [43, 6, 1, 1]),
    ("G", [35, 4, 1, 1]),
    ("F", [16, 2, 1, 1]),
    ("I", [81, 22, 4, 1, 1, 1]),
    ("H", [26, 2, 1]),
    ("K", [11, 1, 1, 1]),
    ("J", [3, 1]),
    ("M", [29, 4, 1, 1]),
    ("L", [53, 10, 1, 1]),
    ("O", [58, 13, 2, 1]),
    ("N", [69, 16, 2, 1, 1]),
    ("P", [32, 4, 1, 1]),
    ("S", [82, 23, 4, 1, 1, 1]),
    ("R", [73, 14, 1, 1]),
    ("U", [36, 3, 1, 1]),
    ("T", [68, 15, 2, 1]),
    ("W", [11, 1, 1]),
    ("V", [13, 1]),
    ("Y", [18, 1]),
    ("X", [4, 1]),
    ("Z", [5, 1, 1, 1])
    ]

type BoardGen = State (StdGen, Freqs) 

randomLetter :: BoardGen BS.ByteString
randomLetter = state randLet where
    randLet (g, fs) = (a, (g', fs')) where
        fsum = getSum $ foldMap (Sum . head) fs
        a = findI i $ M.toList fs
        findI j ((c,(p:_)):cs) =
            let j' = j - p
            in if j' <= 0 then c else findI j' cs
        findI _ [(c, _)] = c
        findI _ _ = error "Something went horribly wrong"
        (i, g') = randomR (0, fsum - 1) g
        fs' = M.alter reduce a fs 
        reduce (Just [_]) = Nothing
        reduce (Just (_:xs)) = Just xs
        reduce _ = Nothing

newtype Board = Board (Array (Int, Int) BS.ByteString)

coords :: [(Int, Int)]
coords = [(r,c) | r <- [0..3], c <- [0..3]]

instance Show Board where
    show (Board arr) = foldr toStr [] coords where
        toStr p@(_,c) = shows (arr ! p) .
            if c == 3 then showChar '\n' else showChar ' '

newBoard :: [BS.ByteString] -> Board
newBoard = Board . array ((0,0),(3,3)) . zip coords

-- fix this
randomBoard :: StdGen -> (Board, StdGen)
randomBoard g = (newBoard l, g') where
    (l, (g', _)) = runState brd (g, letterFreqs)
    brd = forM [0..15] $ const (randomLetter)

data Pos = Pos {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Word16
    deriving (Show)

nextPos :: Pos -> [Pos]
nextPos (Pos p@(r,c) bm) = do
    r' <- [-1..1]
    c' <- [-1..1]
    guard (not (r' == 0 && c' == 0))
    let r'' = r + r'
        c'' = c + c'
        i = r'' * 4 + c''
    guard (r'' >= 0 && r'' <= 3)
    guard (c'' >= 0 && c'' <= 3)
    guard (not $ testBit bm i)
    return $ Pos (r'',c'') (bm .|. bit i)

data TreeF a =
    TreeF [([Word8], a)]
  deriving (Functor)

buildTree :: Board -> Maybe Pos -> TreeF (Maybe Pos)
buildTree (Board arr) Nothing = TreeF $ do
    p@(r,c) <- coords
    let i = bit (r * 4 + c)
    return (BS.unpack (arr ! p), Just $ Pos p i)
buildTree (Board arr) (Just pos) = TreeF $ do
    pos'@(Pos p _) <- nextPos pos
    return (BS.unpack (arr ! p), Just pos')

solveTree :: TreeF (Maybe Trie -> [BS.ByteString] -> [BS.ByteString]) ->
    Maybe Trie -> [BS.ByteString] -> [BS.ByteString]
solveTree _ Nothing = id
solveTree (TreeF xs) (Just t) = f where
    f = case getWord t of
        Nothing -> f'
        Just w -> (w:) . f'
    f' bs = foldr subTree bs xs
    subTree (cs, tf) = tf $ descends cs t
    descends [] tr = return tr
    descends (c:cs) tr = do
        tr' <- descendTrie c tr
        descends cs tr'

hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo f g = hylo' where hylo' = g . fmap hylo' . f

solver :: Board -> Trie -> [BS.ByteString]
solver b t = filter ((>=3) . BS.length) $
    hylo (buildTree b) solveTree Nothing (Just t) []

