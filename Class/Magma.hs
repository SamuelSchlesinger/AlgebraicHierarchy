module Class.Magma
  (
    Magma(..)
  ) where

-- An associative binary operation.
-- Obeys no laws.
class Magma m where
  (?) :: m -> m -> m

instance Magma [a] where
  [] ? xs = xs
  (y : ys) ? xs = y : (ys ? xs)
