{-# LANGUAGE Safe #-}

module Data.Trivial
  (
    Trivial(..)
  ) where

data Trivial = Trivial
trivial :: a -> Trivial
trivial _ = Trivial
