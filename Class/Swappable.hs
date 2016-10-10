module Class.Swappable
  ( 
    Swappable(..)
  ) where

-- | Two argument predicates where you can flip the
-- arguments and not affect constructibility. 
--
-- Obeys: swap . swap = id
class Swappable m where
  swap :: m a b -> m b a
