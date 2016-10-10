{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module Class.Isomorphic
  (
    (<~>)(..)
  ) where

import Class.Subset

-- | inject (inject a) ~ a
class (a <: b, b <: a) => a <~> b
