{-# LANGUAGE NoImplicitPrelude, Safe #-}

module Class.Monoid
  ( 
    Monoid(..)
  ) where

import Class.Associative
import Class.Neutral

class (Associative m, Neutral m) => Monoid m

(++) :: Monoid m => m -> m -> m
(++) = (<>)

empty :: Monoid m => m
empty = neutral
