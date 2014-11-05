{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr Program where
  lit k = [PushI k]
  mul x y = x ++ y ++ [Mul]
  add x y = x ++ y ++ [Add]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program
