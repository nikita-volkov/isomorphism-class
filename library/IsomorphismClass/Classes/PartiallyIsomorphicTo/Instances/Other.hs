{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.Other where

import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Primitive.ByteArray
import qualified Data.Sequence
import qualified Data.Vector
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.ByteString ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyByteString ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyText ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyTextBuilder ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.ShortByteString ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.StrictTextBuilder ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String ()
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.Text ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo () sub where
  to = const ()
  partiallyFrom = const Nothing

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance PartiallyIsomorphicTo super Void where
  to = absurd
  partiallyFrom = const Nothing

--

instance PartiallyIsomorphicTo [Word8] ByteString where
  to = Data.ByteString.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.unpack . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo [Word8] TextArray.Array where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo [a] [a] where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [a] (Vector a) where
  to = toList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [a] (Seq a) where
  to = toList
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo ByteString ByteString where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString [Word8] where
  to = Data.ByteString.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo ByteString TextArray.Array where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Lazy.ByteString where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString [Word8] where
  to = Data.ByteString.Lazy.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString where
  to = to . to @ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString TextArray.Array where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Builder.Builder where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder [Word8] where
  to = to . to @ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder ByteString where
  to = Data.ByteString.Builder.byteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Builder.lazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Builder.shortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder TextArray.Array where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.Primitive.ByteArray.ByteArray where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Short.ShortByteString where
  to (Data.ByteString.Short.Internal.SBS array) = Data.Primitive.ByteArray.ByteArray array
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray TextArray.Array where
  to a = Data.Primitive.ByteArray.ByteArray (TextCompatArray.toUnliftedByteArray a)
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo (Vector a) (Vector a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Vector a) [a] where
  to = Data.Vector.fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Vector a) (Seq a) where
  to = to . to @[a]
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (Seq a) (Seq a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Seq a) [a] where
  to = Data.Sequence.fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Seq a) (Vector a) where
  to = to . to @[a]
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (Set a) (Set a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Set Int) IntSet where
  to = fromList . toList
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo IntSet IntSet where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo IntSet (Set Int) where
  to = fromList . toList
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (Map k v) (Map k v) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Map Int v) (IntMap v) where
  to = fromList . toList
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (IntMap a) (IntMap a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (IntMap v) (Map Int v) where
  to = fromList . toList
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (Maybe a) (Maybe a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Either a b) (Either a b) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (First a) (First a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Last a) (Last a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Product a) (Product a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Sum a) (Sum a) where
  to = id
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo Bool Bool where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Char Char where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Double Double where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Float Float where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int Int where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int Word where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int16 Int16 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int16 Word16 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int32 Int32 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int32 Word32 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int64 Int64 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int64 Word64 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int8 Int8 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Int8 Word8 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Integer Integer where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Rational Rational where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word Int where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word Word where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word16 Int16 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word16 Word16 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word32 Int32 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word32 Word32 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word64 Int64 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word64 Word64 where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word8 Int8 where
  to = fromIntegral
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Word8 Word8 where
  to = id
  partiallyFrom = Just . to
