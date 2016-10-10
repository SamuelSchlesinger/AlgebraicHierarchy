module Class.Neutral 
  (
    Neutral(..)
  ) where

import Class.Magma

-- | A Magma which has a neutral element
-- 
-- Obeys: neutral ? m = m
--        m ? neutral = m
class Magma m => Neutral m where
  neutral :: m 
