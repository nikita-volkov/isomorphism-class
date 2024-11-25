{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntMapAndMapOfInt where

import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo (Map Int v) (IntMap v) where
  to = Data.Map.Strict.fromList . Data.IntMap.Strict.toList

instance PartiallyIsomorphicTo (IntMap v) (Map Int v) where
  to = Data.IntMap.Strict.fromList . Data.Map.Strict.toList

instance IsomorphicTo (Map Int v) (IntMap v)

instance IsomorphicTo (IntMap v) (Map Int v)
