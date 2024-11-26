{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndList where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf (Vector a) [a] where
  to = Data.Vector.fromList

instance IsomorphicToSubsetOf [a] (Vector a) where
  to = Data.Vector.toList

instance IsomorphicTo (Vector a) [a]

instance IsomorphicTo [a] (Vector a)
