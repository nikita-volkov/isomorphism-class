{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndSeq where

import qualified Data.Sequence
import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList

instance IsSome (Seq a) (Vector a) where
  to = Data.Sequence.fromList . Data.Vector.toList

instance Is (Vector a) (Seq a)

instance Is (Seq a) (Vector a)
