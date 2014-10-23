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
xor = foldl maybeFlip False

maybeFlip x True = not x
maybeFlip x False = x

mapz :: (a -> b) -> [a] -> [b]
mapz f = foldl (\accum el -> accum ++ [f el]) []
