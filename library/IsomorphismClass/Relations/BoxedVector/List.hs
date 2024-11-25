{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVector.List where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo (Vector a) [a] where
  to = Data.Vector.fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [a] (Vector a) where
  to = Data.Vector.toList
  partiallyFrom = Just . to

instance IsomorphicTo (Vector a) [a]

instance IsomorphicTo [a] (Vector a)
