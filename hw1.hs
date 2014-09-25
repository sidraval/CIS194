toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherBackward :: [Integer] -> [Integer]
doubleEveryOtherBackward (x:y:xs) = [x, y * 2] ++ doubleEveryOtherBackward xs
doubleEveryOtherBackward (x:[]) = [x]
doubleEveryOtherBackward [] = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherBackward . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n first second third
  | n < 2 = [(first, second)]
  | otherwise = hanoi (n - 1) first third second ++
                [(first, second)] ++
                hanoi (n-1) third second first