module Class.Runnable
  (
    Runnable(..)
  ) where

-- | The existence of this function is enough to call yourself
-- runnable, as runnable is merely the set of predicates over
-- types which imply the existence of the type they're parametrized
-- over. You can imagine that it implies that the things you can compute
-- within the predicate are bounded by the things which are computable
-- externally.
class Runnable m where
  run :: m a -> a
