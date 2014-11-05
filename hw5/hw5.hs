{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Parser
import StackVM

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = maybeEval . parseExp Lit Add Mul

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval (Just t) = Just (eval t)
maybeEval Nothing = Nothing

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  mul = Mul
  add = Add

instance Expr Integer where
  lit k = k
  mul x y = x * y
  add x y = x + y

instance Expr Bool where
  lit k
    | k <= 0 = False
    | otherwise = True
  mul x y = x && y
  add x y = x || y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit k = MinMax k
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)
