module Class.Magma
  (
    Magma(..)
  ) where

-- An associative binary operation.
-- Obeys no laws.
class Magma m where
  (?) :: m -> m -> m
