module Class.Group
  (
    Group(..)
  ) where

import Prelude ()
import Class.Monoid 
import Class.Inverse 
import Class.Associative

class (Inverse g, Monoid g) => Group g

(&) :: Group g => g -> g -> g
(&) = (<>)
