import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibl :: [Integer]
fibl = map fib [0..]

fibs2 :: [Integer]
fibs2 = foldr (\el accum -> accum ++ [(sum accum)]) [0,1] [0..]

fib' = sum

{- thing = [ x | x <- [0..], y <- [] ] -}
