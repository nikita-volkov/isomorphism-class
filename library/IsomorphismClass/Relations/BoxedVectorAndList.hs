{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndList where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf (Vector a) [a] where
  to = Data.Vector.fromList

instance IsSubsetOf [a] (Vector a) where
  to = Data.Vector.toList

instance IsEqualTo (Vector a) [a]

instance IsEqualTo [a] (Vector a)
