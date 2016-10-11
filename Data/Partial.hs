{-# LANGUAGE Safe #-}

module Data.Partial
  (
    Partial(..)
  ) where

import Data.Bool

data Partial a = Defined a | Undefined

partial :: b -> (a -> b) -> Partial a -> b
partial b f p = case p of
  Defined a -> f a
  Undefined -> b

qualify :: (a -> Bool) -> a -> Partial a
qualify p a | p a       = Defined a
            | otherwise = Undefined
