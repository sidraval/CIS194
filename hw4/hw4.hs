import Data.List

fun1 :: [Integer] -> Integer
fun1 = foldl (\accum x -> (x-2) * accum)  1 . filter even
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = (+ 1) . sum . takeWhile (/= 1) . iterate collatz

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

{- data Tree a = Leaf | Node Integer (Tree a) a (Tree a) -}
{- foldTree :: [a] -> Tree a -}
{- foldTree = foldr (\accum el -> ) Leaf  -}

xor :: [Bool] -> Bool
xor = foldl (\accum el -> (accum || el) && (not (accum && el))) False

mapz :: (a -> b) -> [a] -> [b]
mapz f = foldl (\accum el -> accum ++ [f el]) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) ([1..n] \\ (constrained n))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

constrained n = map removalForm (filteredNums n)

removalForm (x, y) = x + y + 2 * x * y

filteredNums n = filter (constraints n) $ cartProd [1..n] [1..n]

constraints :: Integer -> (Integer, Integer) -> Bool
constraints n (x, y) = (x <= y) && (x + y + (2 * x * y) <= n)
