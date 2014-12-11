{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Party where

import Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps i) = GL (emp:emps) (i + empFun emp) 

instance Monoid GuestList where
  mappend (GL first first_fun) (GL second second_fun) = GL (first ++ second) (first_fun + second_fun)
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL e1 f1) (GL e2 f2)
  | f1 > f2 = GL e1 f1
  | otherwise = GL e2 f2

treeFold :: (b -> (Tree a) -> b) -> b -> Tree a -> b
treeFold foldFunction accumulator node = treeFold foldFunction (foldFunction accumulator 
