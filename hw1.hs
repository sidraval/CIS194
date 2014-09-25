import Test.HUnit

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

-- Tests

testtoDigits = TestCase $ assertEqual "toDigits" (toDigits 1234) [1,2,3,4]
testtoDigitsRev = TestCase $ assertEqual "toDigitsRev" (toDigitsRev 1234) [4,3,2,1]
testdoubleEveryOther = TestCase $ assertEqual "doubleEveryOther" (doubleEveryOther [1,2,3,4,5]) [1,4,3,8,5]
testsumDigits = TestCase $ assertEqual "sumDigits" (sumDigits [1, 12, 3, 45]) 16
testhanoi = TestCase $ assertEqual "hanoi" (hanoi 2 "a" "b" "c") [("a","c"), ("a","b"), ("c","b")]

tests = TestList [TestLabel "toDigits" testtoDigits,TestLabel "toDigitsRev" testtoDigitsRev, TestLabel "doubleEveryOther" testdoubleEveryOther, TestLabel "sumDigits" testsumDigits, TestLabel "hanoi" testhanoi]