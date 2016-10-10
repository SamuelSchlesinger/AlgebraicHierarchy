{-# LANGUAGE TypeOperators, Safe #-}

module Data.And
  (
    (&)(..)
  ) where

import Class.Swappable

data a & b = a :&: b

and :: (a -> x) -> (b -> y) -> a & b -> x & y
and ax by (a :&: b) = ax a :&: by b

instance Swappable (&) where
  swap (a :&: b) = b :&: a
