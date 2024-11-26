{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ListAndSeq where

import qualified Data.Sequence
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf [a] (Seq a) where
  to = toList

instance IsSubsetOf (Seq a) [a] where
  to = Data.Sequence.fromList

instance IsEqualTo [a] (Seq a)

instance IsEqualTo (Seq a) [a]
