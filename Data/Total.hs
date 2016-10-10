{-# LANGUAGE Safe #-}

module Data.Total
  ( 
    Total(..) 
  , total
  ) where

import Class.Runnable

newtype Total a = Total a

total :: (a -> b) -> Total a -> Total b
total f (Total a) = Total (f a)

instance Runnable Total where
  {-# INLINE run #-}
  run (Total a) = a
