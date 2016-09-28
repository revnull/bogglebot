{-# LANGUAGE FlexibleContexts #-}

module Data.Trie (
                  Trie,
                  readDict,
                  lookupTrie,
                  getWord,
                  descendTrie,
                  sanitize
                  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.ByteString as BS
import Data.ByteString as BSL
import Data.Maybe
import Data.Traversable
import Data.Word

data Trie =
    Empty !(Maybe BS.ByteString)
  | CharTrie !(Maybe BS.ByteString) {-# UNPACK #-} !Word8 Trie
  | Branch !(Maybe BS.ByteString) !(Array Word8 Trie)
  
data STTrie s = 
    STEmpty !(Maybe BS.ByteString)
  | STCharTrie !(Maybe BS.ByteString) {-# UNPACK #-} !Word8 (STTrie s)
  | STBranch !(Maybe BS.ByteString) (STArray s Word8 (STTrie s))

emptyST :: STTrie s
emptyST = STEmpty Nothing

insertWord :: STTrie s -> BS.ByteString -> ST s (STTrie s)
insertWord t bs = ins t $ BS.unpack bs where
    ins (STEmpty _) [] = return $ STEmpty (Just bs)
    ins (STCharTrie _ c t) [] = return $ STCharTrie (Just bs) c t
    ins (STBranch _ arr) [] = return $ STBranch (Just bs) arr
    ins (STEmpty w) (x:xs) = STCharTrie w x <$> ins emptyST xs
    ins (STCharTrie w c t) (x:xs)
        | x == c = STCharTrie w c <$> ins t xs
        | otherwise = do
            arr <- newArray (65,90) emptyST
            writeArray arr c t
            ins emptyST xs >>= writeArray arr x
            return $ STBranch w arr
    ins (STBranch w arr) (x:xs) = STBranch w <$> do
        c <- readArray arr x
        ins c xs >>= writeArray arr x
        return arr

freezeTrie :: STTrie s -> ST s Trie
freezeTrie (STEmpty w) = return (Empty w)
freezeTrie (STCharTrie w c t) = CharTrie w c <$> freezeTrie t
freezeTrie (STBranch w arr) = do
    arr' <- freeze arr
    Branch w <$> traverse freezeTrie arr'

lookupTrie :: Trie -> BS.ByteString -> Bool
lookupTrie t = look t . BS.unpack where
    look (Empty w) [] = isJust w
    look (Empty _) _ = False
    look (CharTrie w _ _) [] = isJust w
    look (CharTrie w c t) (x:xs) = (c == x) && look t xs
    look (Branch w _) [] = isJust w
    look (Branch _ arr) (x:xs) = look (arr ! x) xs

getWord :: Trie -> Maybe BS.ByteString 
getWord (Empty w) = w
getWord (CharTrie w _ _) = w
getWord (Branch w _) = w

descendTrie :: Word8 -> Trie -> Maybe Trie
descendTrie c (Empty _) = Nothing
descendTrie c (CharTrie _ c' t) = guard (c == c') >> return t
descendTrie c (Branch _ arr) = Just (arr ! c)

sanitize :: BSL.ByteString -> Maybe BS.ByteString
sanitize bs = BS.pack <$> traverse san (BSL.unpack bs) where
    san x
      | x >= 65 && x <= 90 = return x
      | x >= 97 && x <= 122 = return (x - 32)
      | otherwise = Nothing

readDict :: FilePath -> IO Trie
readDict path = do
    words <- BSL.split 10 <$> BSL.readFile path
    let words' = catMaybes $ fmap sanitize words
    return $ runST $ foldM insertWord emptyST words' >>= freezeTrie

