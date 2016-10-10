{-# LANGUAGE Safe, TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Class.Subset
  ( 
    (<:)(..) 
  ) where

import Data.Partial
import Data.Proxy

-- | An isomorphism which is partial on the way there
class s <: m where
  extract :: m -> Partial s
  inject :: s -> m

inSubset :: (s <: m) => Proxy s -> m -> Bool
inSubset (p :: Proxy s) m = case ((extract m) :: Partial s)  of
  Undefined -> False
  Defined _ -> True


