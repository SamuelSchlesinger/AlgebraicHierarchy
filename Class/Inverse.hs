module Class.Inverse
  ( 
    Inverse(..)
  ) where

import Prelude hiding (Eq(..))
import Class.Neutral
import Class.Magma
import Class.Decidable

class (Neutral m, Magma m) => Inverse m where
  inv :: m -> m

inverseProp :: (Decidable m, Inverse m) => m -> Bool
inverseProp a = inv a ? a == neutral
