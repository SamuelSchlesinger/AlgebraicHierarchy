{-# LANGUAGE EmptyCase, Safe #-}

module Data.Impossible
  (
    Impossible(..)
  , Not
  ) where

data Impossible = Something
absurd :: Impossible -> a
absurd a = case a of

type Not a = a -> Impossible
