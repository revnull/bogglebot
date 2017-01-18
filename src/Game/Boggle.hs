{-# LANGUAGE OverloadedStrings, DeriveFunctor, FlexibleContexts #-}

module Game.Boggle(
                   solver
                  ,Board
                  ,boardLines
                  ,GameState
                  ,newGame
                  ,gameRunning
                  ,getBoard
                  ,scoreWord
                  ,warn
                  ,hasWarned
                  ,wordValue
                  ,endGame
                  ,Trie
                  ) where

import Prelude hiding (foldr)
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Monoid
import Data.Word
import Data.Array
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe
import Data.FixFile
import qualified Data.FixFile.Trie.Light as T

type Trie = T.Trie ()

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

type BoardGen g = State (g, Freqs) 

randomLetter :: RandomGen g => BoardGen g BS.ByteString
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

boardLines :: Board -> [BS.ByteString]
boardLines (Board arr) = xs where
    xs = do
        r <- [0..3]
        return . BS.concat $ do
            c <- [0..3]
            letters (arr ! (r, c))
    letters bs = [bs, if BS.length bs == 1 then "  " else " "]

newBoard :: [BS.ByteString] -> Board
newBoard = Board . array ((0,0),(3,3)) . zip coords

instance Random Board where
    random g = (newBoard l, g') where
        (l, (g', _)) = runState brd (g, letterFreqs)
        brd = forM ([0..15] :: [Int]) $ const randomLetter

    randomR = const random -- randomR doesn't make sense for this type

data Pos = Pos {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Word16
    deriving (Show)

nextPos :: Pos -> [Pos]
nextPos (Pos (r,c) bm) = do
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

solveTree :: Fixed g =>
    TreeF (Maybe (g Trie) -> [Word8] -> [BS.ByteString] -> [BS.ByteString]) ->
    Maybe (g Trie) -> [Word8] -> [BS.ByteString] -> [BS.ByteString]
solveTree _ Nothing _ = id
solveTree (TreeF xs) (Just t) ys = wf where
    wf = case T.value t of
        Nothing -> wf'
        Just () -> ((BS.pack ys):) . wf'
    wf' ws = foldr subTree ws xs
    subTree (cs, tf) = tf (T.descendTrie (BS.pack cs) t) (pushBS cs ys)
    pushBS [] y = y
    pushBS (z:zs) y = pushBS zs (z:y)

solver :: Fixed g => Board -> g Trie -> [BS.ByteString]
solver b t = filter ((>=3) . BS.length) $
    hylo (buildTree b) solveTree Nothing (Just t) [] []

wordValue :: BS.ByteString -> Word32
wordValue = (fibs !!) . (\x -> x - 3) . fromIntegral . BS.length where
    fibs :: [Word32]
    fibs = 1:1:zipWith (+) fibs (tail fibs)

type GameState =
    (StdGen, Maybe (Board, S.Set BS.ByteString, M.Map BS.ByteString
        (S.Set BS.ByteString), Bool))

gameRunning :: (Functor m, MonadState GameState m) => m Bool
gameRunning = uses _2 isJust

newGame :: (Functor m, MonadState GameState m, Fixed g) => g Trie -> m Bool
newGame t = do
    new <- not <$> gameRunning
    when new $ do
        g <- use _1
        let ((b, ws), g') = flip runState g $ do
                bs <- forM [1..5 :: Int] $ \_ -> do
                    b <- state random
                    let ws = solver b t
                    return (b, ws, getSum $ foldMap (Sum . wordValue) ws)
                let (b, ws, _) = maximumBy (compare `on` (^._3)) bs
                return (b, S.fromList ws)
        put (g', Just (b, ws, M.empty, False))
    return new

getBoard :: (Functor m, MonadState GameState m) => m (Maybe Board)
getBoard = do
    r <- gameRunning
    if r
        then uses _2 (^?_Just._1)
        else return Nothing 

scoreWord :: (Functor m, MonadState GameState m) =>
    BS.ByteString -> BS.ByteString -> m ()
scoreWord p w = do
    r <- gameRunning
    when r $ do
        Just (b, ws, ss, sw) <- use _2
        when (S.member w ws) $ do
            let score Nothing = Just (S.singleton w)
                score (Just i) = Just (S.singleton w <> i)
            _2 .= Just (b, S.delete w ws, M.alter score p ss, sw)

warn :: MonadState GameState m => m ()
warn = uses _2 (^?_Just._4) >>= warn' where
    warn' (Just True) =
        fail "Duplicate Warning"
    warn' (Just False) = _2._Just._4 .= True
    warn' _ = return ()

hasWarned :: MonadState GameState m => m Bool
hasWarned = uses _2 (^?_Just._4) >>= return . maybe False id

endGame :: (Functor m, MonadState GameState m) => 
    m (Maybe ([(BS.ByteString, [BS.ByteString], Word32)],[BS.ByteString]))
endGame = gameRunning >>= end where
    end False = return Nothing
    end _ = do
        Just (_, missed, ss, _) <- use _2
        _2 .= Nothing
        let ss' = do
                (p, ws) <- M.toList ss
                return (p, toList ws, getSum $ foldMap scores ws)
        return (Just (ss', S.toList missed))
    scores = Sum . wordValue

