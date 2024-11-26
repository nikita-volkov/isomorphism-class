{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntMapAndMapOfInt where

import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf (Map Int v) (IntMap v) where
  to = Data.Map.Strict.fromList . Data.IntMap.Strict.toList

instance IsSubsetOf (IntMap v) (Map Int v) where
  to = Data.IntMap.Strict.fromList . Data.Map.Strict.toList

instance IsEqualTo (Map Int v) (IntMap v)

instance IsEqualTo (IntMap v) (Map Int v)
