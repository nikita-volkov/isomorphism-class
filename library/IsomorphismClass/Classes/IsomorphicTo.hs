{-# LANGUAGE CPP #-}

module IsomorphismClass.Classes.IsomorphicTo where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.Primitive.ByteArray as PrimitiveByteArray
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import IsomorphismClass.Classes.PartiallyIsomorphicTo
import IsomorphismClass.Prelude
#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- | Bidirectional conversion between two types with no loss of information.
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsomorphicTo a b@ as \"/B/ is isomorphic to /A/\".
--
-- __Laws__
--
-- /B/ is isomorphic to /A/ if and only if there exists a conversion from /B/
-- to /A/ ('to') and a conversion from /A/ to /B/ ('from') such that:
--
-- - @'from' . 'to' = 'id'@ - For all values of /B/ converting from /B/ to /A/
--     and then converting from /A/ to /B/ produces a value that is identical
--     to the original.
--
-- - @'to' . 'from' = 'id'@ - For all values of /A/ converting from /A/ to /B/
--     and then converting from /B/ to /A/ produces a value that is identical
--     to the original.
--
-- __Usage__
--
-- This class is particularly easy to use in combination with
-- the @TypeApplications@ extension making it clear to the reader what sort
-- of conversion he sees. E.g.,
--
-- > fromString = from @String
--
-- > toText = to @Text
--
-- The types are also self-evident:
--
-- > > :t from @String
-- > from @String :: IsomorphicTo b String => String -> b
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
--
-- __Instance Definition__
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define two instances, namely: @IsomorphicTo A B@ and @IsomorphicTo
-- B A@.
class (PartiallyIsomorphicTo a b, IsomorphicTo b a) => IsomorphicTo a b

instance IsomorphicTo String Text

instance IsomorphicTo String TextLazy.Text

instance IsomorphicTo String TextLazyBuilder.Builder
#if MIN_VERSION_text(2,0,2)
instance IsomorphicTo String TextEncoding.StrictBuilder
#endif
instance IsomorphicTo [Word8] ByteString

instance IsomorphicTo [Word8] ByteStringLazy.ByteString

instance IsomorphicTo [Word8] ByteStringShort.ShortByteString

instance IsomorphicTo [Word8] ByteStringBuilder.Builder

instance IsomorphicTo [Word8] PrimitiveByteArray.ByteArray
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo [Word8] TextArray.Array
#endif 
instance IsomorphicTo [a] [a]

instance IsomorphicTo [a] (Vector a)

instance IsomorphicTo [a] (Seq a)

instance IsomorphicTo Text Text

instance IsomorphicTo Text String

instance IsomorphicTo Text TextLazy.Text

instance IsomorphicTo Text TextLazyBuilder.Builder
#if MIN_VERSION_text(2,0,2)
instance IsomorphicTo Text TextEncoding.StrictBuilder
#endif
instance IsomorphicTo TextLazy.Text TextLazy.Text

instance IsomorphicTo TextLazy.Text String

instance IsomorphicTo TextLazy.Text Text

instance IsomorphicTo TextLazy.Text TextLazyBuilder.Builder

instance IsomorphicTo TextLazy.Text TextEncoding.StrictBuilder

instance IsomorphicTo TextLazyBuilder.Builder TextLazyBuilder.Builder

instance IsomorphicTo TextLazyBuilder.Builder String

instance IsomorphicTo TextLazyBuilder.Builder Text

instance IsomorphicTo TextLazyBuilder.Builder TextLazy.Text

instance IsomorphicTo TextLazyBuilder.Builder TextEncoding.StrictBuilder
#if MIN_VERSION_text(2,0,2)
instance IsomorphicTo TextEncoding.StrictBuilder TextEncoding.StrictBuilder
instance IsomorphicTo TextEncoding.StrictBuilder String
instance IsomorphicTo TextEncoding.StrictBuilder Text
instance IsomorphicTo TextEncoding.StrictBuilder TextLazy.Text
instance IsomorphicTo TextEncoding.StrictBuilder TextLazyBuilder.Builder
#endif
instance IsomorphicTo ByteString ByteString

instance IsomorphicTo ByteString [Word8]

instance IsomorphicTo ByteString ByteStringLazy.ByteString

instance IsomorphicTo ByteString ByteStringShort.ShortByteString

instance IsomorphicTo ByteString ByteStringBuilder.Builder

instance IsomorphicTo ByteString PrimitiveByteArray.ByteArray
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo ByteString TextArray.Array
#endif
instance IsomorphicTo ByteStringLazy.ByteString ByteStringLazy.ByteString

instance IsomorphicTo ByteStringLazy.ByteString [Word8]

instance IsomorphicTo ByteStringLazy.ByteString ByteString

instance IsomorphicTo ByteStringLazy.ByteString ByteStringShort.ShortByteString

instance IsomorphicTo ByteStringLazy.ByteString ByteStringBuilder.Builder

instance IsomorphicTo ByteStringLazy.ByteString PrimitiveByteArray.ByteArray
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo ByteStringLazy.ByteString TextArray.Array
#endif
instance IsomorphicTo ByteStringShort.ShortByteString ByteStringShort.ShortByteString

instance IsomorphicTo ByteStringShort.ShortByteString [Word8]

instance IsomorphicTo ByteStringShort.ShortByteString ByteString

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringLazy.ByteString

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringBuilder.Builder

instance IsomorphicTo ByteStringShort.ShortByteString PrimitiveByteArray.ByteArray
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo ByteStringShort.ShortByteString TextArray.Array
#endif
instance IsomorphicTo ByteStringBuilder.Builder ByteStringBuilder.Builder

instance IsomorphicTo ByteStringBuilder.Builder [Word8]

instance IsomorphicTo ByteStringBuilder.Builder ByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringLazy.ByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringShort.ShortByteString

instance IsomorphicTo ByteStringBuilder.Builder PrimitiveByteArray.ByteArray
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo ByteStringBuilder.Builder TextArray.Array
#endif
instance IsomorphicTo PrimitiveByteArray.ByteArray PrimitiveByteArray.ByteArray

instance IsomorphicTo PrimitiveByteArray.ByteArray [Word8]

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringShort.ShortByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringLazy.ByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringBuilder.Builder
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo PrimitiveByteArray.ByteArray TextArray.Array
#endif
#if !MIN_VERSION_text(2,1,0)
instance IsomorphicTo TextArray.Array [Word8]
instance IsomorphicTo TextArray.Array PrimitiveByteArray.ByteArray
instance IsomorphicTo TextArray.Array ByteStringShort.ShortByteString
instance IsomorphicTo TextArray.Array ByteString
instance IsomorphicTo TextArray.Array ByteStringLazy.ByteString
instance IsomorphicTo TextArray.Array ByteStringBuilder.Builder
#endif
instance IsomorphicTo (Vector a) (Vector a)

instance IsomorphicTo (Vector a) [a]

instance IsomorphicTo (Vector a) (Seq a)

instance IsomorphicTo (Seq a) (Seq a)

instance IsomorphicTo (Seq a) [a]

instance IsomorphicTo (Seq a) (Vector a)

instance IsomorphicTo (Set a) (Set a)

instance IsomorphicTo (Set Int) IntSet

instance IsomorphicTo IntSet IntSet

instance IsomorphicTo IntSet (Set Int)

instance IsomorphicTo (Map k v) (Map k v)

instance IsomorphicTo (Map Int v) (IntMap v)

instance IsomorphicTo (IntMap a) (IntMap a)

instance IsomorphicTo (IntMap v) (Map Int v)

instance IsomorphicTo (Maybe a) (Maybe a)

instance IsomorphicTo (Either a b) (Either a b)

instance IsomorphicTo (First a) (First a)

instance IsomorphicTo (Last a) (Last a)

instance IsomorphicTo (Product a) (Product a)

instance IsomorphicTo (Sum a) (Sum a)

instance IsomorphicTo Bool Bool

instance IsomorphicTo Char Char

instance IsomorphicTo Double Double

instance IsomorphicTo Float Float

instance IsomorphicTo Int Int

instance IsomorphicTo Int Word

instance IsomorphicTo Int16 Int16

instance IsomorphicTo Int16 Word16

instance IsomorphicTo Int32 Int32

instance IsomorphicTo Int32 Word32

instance IsomorphicTo Int64 Int64

instance IsomorphicTo Int64 Word64

instance IsomorphicTo Int8 Int8

instance IsomorphicTo Int8 Word8

instance IsomorphicTo Integer Integer

instance IsomorphicTo Rational Rational

instance IsomorphicTo Word Int

instance IsomorphicTo Word Word

instance IsomorphicTo Word16 Int16

instance IsomorphicTo Word16 Word16

instance IsomorphicTo Word32 Int32

instance IsomorphicTo Word32 Word32

instance IsomorphicTo Word64 Int64

instance IsomorphicTo Word64 Word64

instance IsomorphicTo Word8 Int8

instance IsomorphicTo Word8 Word8

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromText :: IsomorphicTo Text a => Text -> a
-- > fromText = from @Text
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: (IsomorphicTo a b) => b -> a
from = to
