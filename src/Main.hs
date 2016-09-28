{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Trie
import Game.Boggle
import System.Random

main = do 
    d <- readDict "/usr/share/dict/words"
    g <- newStdGen
    print "BoggleBot"
    let (rb, _) = randomBoard g
    print rb
    print (solver rb d)
    
