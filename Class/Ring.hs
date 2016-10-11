{-# LANGUAGE FlexibleContexts #-}

module Class.Ring
  ( 
    Ring(..)
  ) where

import Class.Distributive
import Class.Group

class (Group (Add r), Distributive r) => Ring r
