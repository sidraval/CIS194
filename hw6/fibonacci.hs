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

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

streamFromList :: [a] -> Stream a
streamFromList (x:xs) = Cons x (streamFromList xs)

instance Show a => Show (Stream a) where
  show xs = show $ take 20 $ streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromList [0..]

ruler :: Stream Integer
