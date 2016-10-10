module Class.Abelian
  (
    Abelian(..)
  ) where

import Class.Associative
import Class.Commutative

class (Associative m, Commutative m) => Abelian m
