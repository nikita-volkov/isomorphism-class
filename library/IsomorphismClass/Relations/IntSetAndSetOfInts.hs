{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntSetAndSetOfInts where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo (Set Int) IntSet where
  to = fromList . toList

instance PartiallyIsomorphicTo IntSet (Set Int) where
  to = fromList . toList

instance IsomorphicTo (Set Int) IntSet

instance IsomorphicTo IntSet (Set Int)
