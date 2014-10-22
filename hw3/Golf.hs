{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List
import Test.HUnit
import Test.QuickCheck

skips :: [a] -> [[a]]
skips xs = map (\n -> everyNth xs n) [1..(length xs)]

everyNth xs n
  | (length xs) < n = []
  | otherwise = take 1 (drop (n-1) xs) ++ (everyNth (drop n xs) n)

isLocalMaxima :: [Integer] -> [Integer]
isLocalMaxima (x:y:z:_)
  | x < y && z < y = [y]
  | otherwise = []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:others) = isLocalMaxima (x:y:z:[]) ++ localMaxima (y:z:others)
localMaxima (x:_) = []

histogram :: [Integer] -> String
histogram = unlines . map oneLine . mappedData

mappedData :: [Integer] -> [[Integer]]
mappedData xs = filter (not . null) $ foldl (\accum el -> [reduceData $ take 1 accum] ++ accum) [xs] xs

reduceData :: [[Integer]] -> [Integer]
reduceData (x:xs) = foldl (\accum el -> delete el accum) x [0..9]

oneLine :: [Integer] -> String
oneLine dataSet = map (starOrEmpty dataSet) [0..9]

starOrEmpty :: [Integer] -> Integer -> Char
starOrEmpty y x
  | x `elem` y = '*'
  | otherwise = ' '

-- QuickCheck Invariants

invarianceOfLength s = length (skips s) == length s
firstElement s
  | length s > 0 = head (skips s) == s
  | otherwise = skips s == []
 
lastElement s
  | length s > 0 = head (last (skips s)) == last s
  | otherwise = skips s == []
