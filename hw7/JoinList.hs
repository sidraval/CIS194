module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) first second = Append (mappend (tag first) (tag second)) first second

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty
tag (Single ann char) = ann
tag (Append ann _ _) = ann

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single m a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append m first second)
  | i <= (getSize . size $ tag first) = indexJ i first
  | otherwise = indexJ (i - (getSize $ size m)) second

instance Sized Int where
  size = Size

instance Monoid Int where
  mempty = 0
  mappend = (+)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i (Single m a)
  | i > 0 = Empty
  | otherwise = Single m a
dropJ i (Append m first second)
  | i == 0 = Append m first second
  | i > (getSize . size $ tag first) = (Append m Empty (dropJ (i - (getSize $ size m)) second))
  | otherwise = (Append m (dropJ i first) second)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
