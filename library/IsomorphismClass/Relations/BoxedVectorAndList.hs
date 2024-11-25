{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndList where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo (Vector a) [a] where
  to = Data.Vector.fromList

instance PartiallyIsomorphicTo [a] (Vector a) where
  to = Data.Vector.toList

instance IsomorphicTo (Vector a) [a]

instance IsomorphicTo [a] (Vector a)
