{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies #-}

module Class.Field 
  (
    Field(..)
  ) where

import Class.Distributive
import Class.Subset
import Class.Ring

class (Ring r, MulGroup r <: Mul r) => Field r where
  type MulGroup r
