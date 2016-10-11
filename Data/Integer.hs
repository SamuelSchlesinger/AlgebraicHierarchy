{-# LANGUAGE FlexibleInstances, TypeOperators, TypeFamilies, MultiParamTypeClasses #-}

module Data.Integer
  (
    Integer
  ) where

import Class.Magma
import Class.Group
import Class.Monoid
import Class.Associative
import Class.Field
import Class.Ring
import Class.Distributive
import Class.Isomorphic
import Class.Neutral
import Class.Inverse
import Class.Subset
import Data.Partial
import Data.Bool
import qualified Prelude as P
import Prelude (Integer)

newtype Multiplicative a = M a 
newtype Additive a = A a
 
instance Magma (Multiplicative Integer) where
  (M a) ? (M b) = M (a P.* b)

instance Magma (Additive Integer) where
  (A a) ? (A b) = A (a P.+ b)

instance Neutral (Multiplicative Integer) where
  neutral = M 1

instance Neutral (Additive Integer) where
  neutral = A 0

instance Associative (Multiplicative Integer)

instance Associative (Additive Integer)

instance Monoid (Multiplicative Integer)

instance Monoid (Additive Integer)

instance Inverse (Additive Integer) where
  inv (A n) = A (-n)

instance Group (Additive Integer)

instance Multiplicative Integer <: Integer where
  extract n = Defined (M n)
  inject (M n) = n  

instance Integer <: Multiplicative Integer where
  extract (M n) = Defined n
  inject n = M n
 
instance Additive Integer <: Integer where
  extract n = Defined (A n)
  inject (A n) = n

instance Integer <: Additive Integer where
  extract (A n) = Defined n
  inject n = A n

instance Integer <~> Additive Integer
instance Additive Integer <~> Integer
instance Integer <~> Multiplicative Integer
instance Multiplicative Integer <~> Integer

instance Distributive Integer where
  type Mul Integer = Multiplicative Integer
  type Add Integer = Additive Integer


