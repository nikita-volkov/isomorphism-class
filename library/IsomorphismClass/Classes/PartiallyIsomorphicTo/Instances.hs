{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances where

import qualified Data.Sequence
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo a a where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo () sub where
  to = const ()
  partiallyFrom = const Nothing

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance PartiallyIsomorphicTo super Void where
  to = absurd
  partiallyFrom = const Nothing

instance PartiallyIsomorphicTo [a] (Seq a) where
  to = toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Seq a) [a] where
  to = Data.Sequence.fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Set Int) IntSet where
  to = fromList . toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo IntSet (Set Int) where
  to = fromList . toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Map Int v) (IntMap v) where
  to = fromList . toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (IntMap v) (Map Int v) where
  to = fromList . toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int Word where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int16 Word16 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int32 Word32 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int64 Word64 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int8 Word8 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word Int where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word16 Int16 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word32 Int32 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word64 Int64 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word8 Int8 where
  to = fromIntegral
  partiallyFrom = Just . to
