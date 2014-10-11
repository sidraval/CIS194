{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List

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
mappedData xs = filter (not . null) $ foldl (\accum el -> [reduceData . head $ accum] ++ accum) [xs] xs

reduceData :: [Integer] -> [Integer]
reduceData xs = foldl (\accum el -> delete el accum) xs [0..9]

oneLine :: [Integer] -> String
oneLine dataSet = map (starOrEmpty dataSet) [0..9]

starOrEmpty :: [Integer] -> Integer -> Char
starOrEmpty y x
  | x `elem` y = '*'
  | otherwise = ' '
