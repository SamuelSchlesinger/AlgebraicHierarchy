{-# LANGUAGE Safe, NoImplicitPrelude, RankNTypes, TypeOperators #-}

module Class.Decidable
  ( 
    Decidable(..)
  ) where

import Prelude (not)
import qualified Prelude as P
import Data.Bool
import Data.Int
import Class.Subset

class Decidable a where
  (==) :: a -> a -> Bool
  (!=) :: a -> a -> Bool
  a != b = not (a == b)
