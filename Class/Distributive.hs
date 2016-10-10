{-# LANGUAGE TypeFamilies, UndecidableInstances, TypeOperators, FlexibleContexts ,ScopedTypeVariables#-}

module Class.Distributive
  ( ) where

import Class.Isomorphic
import Class.Subset
import Class.Magma
import Prelude ()

class
  ( Add r <~> r
  , Mul r <~> r
  , Magma (Add r)
  , Magma (Mul r)
  ) => Distributive r where
  type Add r
  type Mul r

(+) :: Distributive r => r -> r -> r
(a :: r) + b  = (inject (((inject a) :: Add r) ? ((inject b) :: Add r))) :: r

(*) :: Distributive r => r -> r -> r
(a :: r) * b = (inject (((inject) :: Mul r) ? ((inject b) :: Mul r))) :: r
