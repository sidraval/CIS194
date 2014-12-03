{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Sized

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
 | c == 'a' = Score 1
 | c == 'b' = Score 3
 | c == 'c' = Score 3
 | c == 'd' = Score 2
 | c == 'e' = Score 1
 | c == 'f' = Score 4
 | c == 'g' = Score 2
 | c == 'h' = Score 4
 | c == 'i' = Score 1
 | c == 'j' = Score 8
 | c == 'k' = Score 5
 | c == 'l' = Score 1
 | c == 'm' = Score 3
 | c == 'n' = Score 1
 | c == 'o' = Score 1
 | c == 'p' = Score 3
 | c == 'q' = Score 10
 | c == 'r' = Score 1
 | c == 's' = Score 1
 | c == 't' = Score 1
 | c == 'u' = Score 1
 | c == 'v' = Score 4
 | c == 'w' = Score 4
 | c == 'x' = Score 8
 | c == 'y' = Score 4
 | c == 'z' = Score 10
 | otherwise = Score 0

scoreString :: String -> Score
scoreString string = foldl (\accum el -> mappend (score el) accum) (Score 0) string
