{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- fmap :: (a -> b) -> Parser a -> Parser b
instance Functor Parser where
  fmap f (Parser { runParser = rpa }) = Parser { runParser = fmap (first f) . rpa }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser f
    where
      f xs = Just (a, xs)
  -- p1 <*> p2 :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser f
    where
      f xs = case (runParser p1 xs) of
        Nothing -> Nothing
        Just (fab, s) -> fmap (first fab) . (runParser p2) $ s
        -- fab :: a -> b
        -- first fab :: (a, c) -> (b, c)
        -- fmap (first fab) :: Maybe (a, c) -> Maybe (b, c)
        -- (runParser p2) :: String -> Maybe (a, c)
        -- fmap (first fab) . (runParser p2) :: String -> Maybe (b, c) ##### PARSER FUNCTION!

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> (satisfy (== 'a')) <*> (satisfy (== 'b'))

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (satisfy (== 'a')) <*> (satisfy (== 'b'))

intPair :: Parser [Integer]
intPair = (\x y z -> [x,z]) <$> posInt <*> (satisfy (== ' ')) <*> posInt
