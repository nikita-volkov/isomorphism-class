{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndSeq where

import qualified Data.Sequence
import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList

instance IsomorphicToSubsetOf (Seq a) (Vector a) where
  to = Data.Sequence.fromList . Data.Vector.toList

instance IsomorphicTo (Vector a) (Seq a)

instance IsomorphicTo (Seq a) (Vector a)
