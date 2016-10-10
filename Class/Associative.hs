module Class.Associative
  (
    Associative(..)
  , (<>)
  ) where

import Class.Magma

-- | A type with an associative binary operation
-- 
-- Obeys: a <> (b <> c) = (a <> b) <> c
class Magma m => Associative m

(<>) :: Associative m => m -> m -> m
(<>) = (?)
