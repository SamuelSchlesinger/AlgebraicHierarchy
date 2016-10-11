{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies, NoImplicitPrelude, ScopedTypeVariables #-}

module Class.Field 
  (
    Field(..)
  , (/)
  ) where

import Class.Distributive
import Class.Commutative
import Class.Group
import Class.Inverse
import Class.Subset
import Class.Ring
import Class.Associative

import Data.Partial

class (Ring r, Commutative (Add r), Group (MulGroup r), MulGroup r <: r) => Field r where
  type MulGroup r

(/) :: Field r => r -> r -> Partial r
(a :: r) / b =  case (extract b) :: Partial (MulGroup r) of
  Defined b -> case (extract a) :: Partial (MulGroup r) of
    Defined a -> Defined (inject (a <> inv b))
    Undefined -> Undefined
  Undefined -> Undefined
