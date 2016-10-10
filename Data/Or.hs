{-# LANGUAGE TypeOperators, Safe #-}

module Data.Or
  ( 
    (||)(..)
  ) where

import Class.Swappable

data a || b = L a | R b

or :: (a -> x) -> (b -> y) -> a || b -> x || y
or ax by o = case o of
  L a -> L (ax a)
  R b -> R (by b)

instance Swappable (||) where
  swap o = case o of
    L a -> R a
    R b -> L b
