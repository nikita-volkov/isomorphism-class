{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndSeq where

import qualified Data.Sequence
import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Seq a) (Vector a) where
  to = Data.Sequence.fromList . Data.Vector.toList
  partiallyFrom = Just . to

instance IsomorphicTo (Vector a) (Seq a)

instance IsomorphicTo (Seq a) (Vector a)
