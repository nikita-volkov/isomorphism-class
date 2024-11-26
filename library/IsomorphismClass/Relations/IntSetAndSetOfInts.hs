{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntSetAndSetOfInts where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf (Set Int) IntSet where
  to = fromList . toList

instance IsSubsetOf IntSet (Set Int) where
  to = fromList . toList

instance IsEqualTo (Set Int) IntSet

instance IsEqualTo IntSet (Set Int)
