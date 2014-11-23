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
