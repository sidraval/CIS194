{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Parser
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

data VarExprT = Lit1 Integer 
              | Add1 VarExprT VarExprT 
              | Mul1 VarExprT VarExprT 
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit1
  mul = Mul1
  add = Add1

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = M.lookup str
  
--function f give it a map and itll return an integer  
  
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = M.lookup . show
-- mul x y
--   add x y = x
--
--test

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
