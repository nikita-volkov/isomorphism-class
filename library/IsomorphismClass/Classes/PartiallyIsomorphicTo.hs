{-# LANGUAGE CPP #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.ByteString.Short.Internal as ByteStringShortInternal
import qualified Data.Primitive.ByteArray as PrimitiveByteArray
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector as Vector
import IsomorphismClass.Prelude
#if !MIN_VERSION_text(2,1,0)
import qualified Data.Text.Array as TextArray
import qualified IsomorphismClass.TextCompat.Array as TextCompatArray
#endif
#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- |
-- Evidence that @sub@ is a proper subset of @super@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- - @'partiallyFrom' . 'to' = 'Just'@ - For all values of @sub@ converting @sub@ to @super@ and then and attempting to convert back to @sub@ always succeeds and produces a value that is identical to the original.
--
-- - @\a -> fmap 'to' ('partiallyFrom' a) = fmap (const a) ('partiallyFrom' a)@ - For all values of @super@ attempting to convert to @sub@ and then convert back on success produces the same result as the original if the conversion succeeds.
class PartiallyIsomorphicTo super sub where
  to :: sub -> super
  partiallyFrom :: super -> Maybe sub

instance PartiallyIsomorphicTo () sub where
  to = const ()
  partiallyFrom = const Nothing

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance PartiallyIsomorphicTo super Void where
  to = absurd
  partiallyFrom = const Nothing

--

instance PartiallyIsomorphicTo String Text where
  to = Text.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo String TextLazy.Text where
  to = TextLazy.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo String TextLazyBuilder.Builder where
  to = TextLazy.unpack . TextLazyBuilder.toLazyText
  partiallyFrom = Just . to

#if MIN_VERSION_text(2,0,2)
instance PartiallyIsomorphicTo String TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo [Word8] ByteString where
  to = ByteString.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] ByteStringLazy.ByteString where
  to = ByteStringLazy.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] ByteStringShort.ShortByteString where
  to = ByteStringShort.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] ByteStringBuilder.Builder where
  to = ByteStringLazy.unpack . ByteStringBuilder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] PrimitiveByteArray.ByteArray where
  to = toList
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo [Word8] TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString
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

instance PartiallyIsomorphicTo Text Text where
  to = id
  partiallyFrom = Just . to

-- | Performs replacement on invalid Unicode chars in the string.
instance PartiallyIsomorphicTo Text String where
  to = Text.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Text TextLazy.Text where
  to = TextLazy.toStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Text TextLazyBuilder.Builder where
  to = TextLazy.toStrict . TextLazyBuilder.toLazyText
  partiallyFrom = Just . to

#if MIN_VERSION_text(2,0,2)
instance PartiallyIsomorphicTo Text TextEncoding.StrictBuilder where
  to = TextEncoding.strictBuilderToText
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo TextLazy.Text TextLazy.Text where
  to = id
  partiallyFrom = Just . to

-- | Performs replacement on invalid Unicode chars in the string.
instance PartiallyIsomorphicTo TextLazy.Text String where
  to = TextLazy.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextLazy.Text Text where
  to = TextLazy.fromStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextLazy.Text TextLazyBuilder.Builder where
  to = TextLazyBuilder.toLazyText
  partiallyFrom = Just . to

#if MIN_VERSION_text(2,0,2)
instance PartiallyIsomorphicTo TextLazy.Text TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo TextLazyBuilder.Builder TextLazyBuilder.Builder where
  to = id
  partiallyFrom = Just . to

-- | Performs replacement on invalid Unicode chars in the string.
instance PartiallyIsomorphicTo TextLazyBuilder.Builder String where
  to = TextLazyBuilder.fromString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextLazyBuilder.Builder Text where
  to = TextLazyBuilder.fromText
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextLazyBuilder.Builder TextLazy.Text where
  to = TextLazyBuilder.fromLazyText
  partiallyFrom = Just . to

#if MIN_VERSION_text(2,0,2)
instance PartiallyIsomorphicTo TextLazyBuilder.Builder TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText
  partiallyFrom = Just . to
#endif

#if MIN_VERSION_text(2,0,2)
--

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextEncoding.StrictBuilder where
  to = id
  partiallyFrom = Just . to

-- | Performs replacement on invalid Unicode chars in the string.
instance PartiallyIsomorphicTo TextEncoding.StrictBuilder String where
  to = TextEncoding.textToStrictBuilder . to
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder Text where
  to = TextEncoding.textToStrictBuilder
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextLazy.Text where
  to = TextEncoding.textToStrictBuilder . to
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextLazyBuilder.Builder where
  to = TextEncoding.textToStrictBuilder . to
  partiallyFrom = Just . to

#endif
--

instance PartiallyIsomorphicTo ByteString ByteString where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString [Word8] where
  to = ByteString.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString ByteStringLazy.ByteString where
  to = ByteStringLazy.toStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString ByteStringShort.ShortByteString where
  to = ByteStringShort.fromShort
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString ByteStringBuilder.Builder where
  to = ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo ByteString TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo ByteStringLazy.ByteString ByteStringLazy.ByteString where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringLazy.ByteString [Word8] where
  to = ByteStringLazy.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringLazy.ByteString ByteString where
  to = ByteStringLazy.fromStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringLazy.ByteString ByteStringShort.ShortByteString where
  to = to . to @ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringLazy.ByteString ByteStringBuilder.Builder where
  to = ByteStringBuilder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringLazy.ByteString PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo ByteStringLazy.ByteString TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString ByteStringShort.ShortByteString where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString [Word8] where
  to = ByteStringShort.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString ByteString where
  to = ByteStringShort.toShort
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString ByteStringLazy.ByteString where
  to = to . to @ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString ByteStringBuilder.Builder where
  to = to . to @ByteStringLazy.ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringShort.ShortByteString PrimitiveByteArray.ByteArray where
  to (PrimitiveByteArray.ByteArray array) = ByteStringShortInternal.SBS array
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo ByteStringShort.ShortByteString TextArray.Array where
  to a = ByteStringShortInternal.SBS (TextCompatArray.toUnliftedByteArray a)
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo ByteStringBuilder.Builder ByteStringBuilder.Builder where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringBuilder.Builder [Word8] where
  to = to . to @ByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringBuilder.Builder ByteString where
  to = ByteStringBuilder.byteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringBuilder.Builder ByteStringLazy.ByteString where
  to = ByteStringBuilder.lazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringBuilder.Builder ByteStringShort.ShortByteString where
  to = ByteStringBuilder.shortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteStringBuilder.Builder PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo ByteStringBuilder.Builder TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray PrimitiveByteArray.ByteArray where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray [Word8] where
  to = fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray ByteStringShort.ShortByteString where
  to (ByteStringShortInternal.SBS array) = PrimitiveByteArray.ByteArray array
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray ByteString where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray ByteStringLazy.ByteString where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray ByteStringBuilder.Builder where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo PrimitiveByteArray.ByteArray TextArray.Array where
  to a = PrimitiveByteArray.ByteArray (TextCompatArray.toUnliftedByteArray a)
  partiallyFrom = Just . to
#endif

--

#if !MIN_VERSION_text(2,1,0)
instance PartiallyIsomorphicTo TextArray.Array [Word8] where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextArray.Array PrimitiveByteArray.ByteArray where
  to (PrimitiveByteArray.ByteArray arr) = TextCompatArray.fromUnliftedByteArray arr
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextArray.Array ByteStringShort.ShortByteString where
  to (ByteStringShortInternal.SBS arr) = TextCompatArray.fromUnliftedByteArray arr
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextArray.Array ByteString where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextArray.Array ByteStringLazy.ByteString where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo TextArray.Array ByteStringBuilder.Builder where
  to = to . to @ByteStringShort.ShortByteString
  partiallyFrom = Just . to
#endif

--

instance PartiallyIsomorphicTo (Vector a) (Vector a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Vector a) [a] where
  to = Vector.fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Vector a) (Seq a) where
  to = to . to @[a]
  partiallyFrom = Just . to

--

instance PartiallyIsomorphicTo (Seq a) (Seq a) where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo (Seq a) [a] where
  to = Seq.fromList
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
