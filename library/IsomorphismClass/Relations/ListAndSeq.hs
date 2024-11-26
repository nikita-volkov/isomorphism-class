{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ListAndSeq where

import qualified Data.Sequence
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf [a] (Seq a) where
  to = toList

instance IsomorphicToSubsetOf (Seq a) [a] where
  to = Data.Sequence.fromList

instance IsomorphicTo [a] (Seq a)

instance IsomorphicTo (Seq a) [a]
