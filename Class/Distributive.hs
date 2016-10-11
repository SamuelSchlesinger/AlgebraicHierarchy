{-# LANGUAGE TypeFamilies, UndecidableInstances, TypeOperators, FlexibleContexts ,ScopedTypeVariables#-}

module Class.Distributive
  ( 
    Distributive(..)
  ) where

import Class.Isomorphic
import Class.Subset
import Class.Monoid
import Class.Magma
import Prelude ()

-- A Semiring
class
  ( Add r <~> r
  , Mul r <~> r
  , Monoid (Add r)
  , Monoid (Mul r)
  ) => Distributive r where
  type Add r
  type Mul r

(+) :: Distributive r => r -> r -> r
(a :: r) + b  = (inject (((inject a) :: Add r) ? ((inject b) :: Add r))) :: r

(*) :: Distributive r => r -> r -> r
(a :: r) * b = (inject (((inject a) :: Mul r) ? ((inject b) :: Mul r))) :: r

