import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibl :: [Integer]
fibl = map fib [0..]

{- fibs2 :: [Integer] -}
{- fibs2 = foldr (\el accum -> (fib' $ reverse accum):(reverse accum)) [0,1] [0..] -}

fibs2' [] = [0]
fibs2' [0,1] = [0,1]
fibs2' xs = (xs ++ [fib' $ reverse xs])

fib' = sum . take 2
