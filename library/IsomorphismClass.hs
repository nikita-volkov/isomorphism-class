-- |
-- = The ultimate solution to the Conversion Problem
--
-- By Conversion Problem we mean a repetitive chain of robotic actions one has
-- to take to translate data from one form to another without losing
-- information. This problem is often extended by packages using different
-- conventions. This library addresses all those problems by abstracting over
-- patterns and providing a standard, concise and clear interface.
--
-- How often do you import @Data.Text.Lazy@ only to call
-- its 'Data.Text.Lazy.fromStrict'? How about importing @Data.Text@ only to
-- to call its 'Data.Text.unpack'? How about going thru the always fun
-- sequence of importing @Data.ByteString.Builder@ only to to call its
-- 'Data.ByteString.Builder.toLazyByteString' and then importing
-- @Data.ByteString.Lazy@ only to call its 'Data.ByteString.Lazy.toStrict'?
--
-- Those all are instances of one pattern. They are conversions between
-- representations of the same information. Information being the same
-- implies that none of it gets lost during the conversions. The zero loss
-- can be proven by being able to restore data identical to the original from
-- its transformed representation.
--
-- Turns out there usually can only be one way of defining such a conversion
-- between two types. This makes a huge deal in terms of how clear it is what
-- the conversion does. It helps the library authors ensure that they define
-- something that makes sense and provides insight to the users about what
-- the conversions do. We capture this with a lawful class, which ensures
-- that there is only one proper way of defining an instance for it. However
-- despite the strict rules, there's a lot of instances of this pattern. So
-- it is both universal and clear.
--
-- Here's a few samples of what this library lets you do:
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'TextLazyBuilder.Builder' $
--     "Height of " <> 'to' name <> " is " <> 'showAs' height
-- @
--
-- @
-- combineEncodings :: 'ByteStringShort.ShortByteString' -> 'PrimitiveByteArray.ByteArray' -> 'ByteString' -> [Word8]
-- combineEncodings a b c =
--   'from' @'ByteStringBuilder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
module IsomorphismClass
  ( -- * Typeclass
    IsomorphicTo (..),
    from,

    -- * Common Utilities
    showAs,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.ByteString.Short.Internal as ByteStringShortInternal
import qualified Data.Primitive.ByteArray as PrimitiveByteArray
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VectorGeneric
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array as TextCompatArray

-- | Bidirectional conversion between two types with no loss of information.
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsomorphicTo a b@ as \"/B/ is isomorphic to /A/\".
--
-- __Laws__
--
-- /A/ is isomorphic to /B/ if and only if there exists a conversion from /A/
-- to /B/ ('to') and a conversion from /B/ to /A/ ('from') such that:
--
-- - @'from' . 'to' = 'id'@ - For all values of /A/ converting from /A/ to /B/
--     and then converting from /B/ to /A/ produces a value that is identical
--     to the original.
--
-- - @'to' . 'from' = 'id'@ - For all values of /B/ converting from /B/ to /A/
--     and then converting from /A/ to /B/ produces a value that is identical
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
-- > from @String :: IsomorphicTo String b => String -> b
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
--
-- __Instance Definition__
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define two instances, namely: @IsomorphicTo A B@ and @IsomorphicTo
-- B A@.
class IsomorphicTo b a => IsomorphicTo a b where
  to :: b -> a

--

instance IsomorphicTo String Text where
  to = Text.unpack

instance IsomorphicTo String TextLazy.Text where
  to = TextLazy.unpack

instance IsomorphicTo String TextLazyBuilder.Builder where
  to = TextLazy.unpack . TextLazyBuilder.toLazyText

--

instance IsomorphicTo [Word8] ByteString where
  to = ByteString.unpack

instance IsomorphicTo [Word8] ByteStringLazy.ByteString where
  to = ByteStringLazy.unpack

instance IsomorphicTo [Word8] ByteStringShort.ShortByteString where
  to = ByteStringShort.unpack

instance IsomorphicTo [Word8] ByteStringBuilder.Builder where
  to = ByteStringLazy.unpack . ByteStringBuilder.toLazyByteString

instance IsomorphicTo [Word8] PrimitiveByteArray.ByteArray where
  to = toList

instance IsomorphicTo [Word8] TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString

--

instance IsomorphicTo [a] [a] where
  to = id

instance IsomorphicTo [a] (Vector a) where
  to = toList

instance IsomorphicTo [a] (Seq a) where
  to = toList

--

instance IsomorphicTo Text Text where
  to = id

instance IsomorphicTo Text String where
  to = Text.pack

instance IsomorphicTo Text TextLazy.Text where
  to = TextLazy.toStrict

instance IsomorphicTo Text TextLazyBuilder.Builder where
  to = TextLazy.toStrict . TextLazyBuilder.toLazyText

--

instance IsomorphicTo TextLazy.Text TextLazy.Text where
  to = id

instance IsomorphicTo TextLazy.Text String where
  to = TextLazy.pack

instance IsomorphicTo TextLazy.Text Text where
  to = TextLazy.fromStrict

instance IsomorphicTo TextLazy.Text TextLazyBuilder.Builder where
  to = TextLazyBuilder.toLazyText

--

instance IsomorphicTo TextLazyBuilder.Builder TextLazyBuilder.Builder where
  to = id

instance IsomorphicTo TextLazyBuilder.Builder String where
  to = TextLazyBuilder.fromString

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = TextLazyBuilder.fromText

instance IsomorphicTo TextLazyBuilder.Builder TextLazy.Text where
  to = TextLazyBuilder.fromLazyText

--

instance IsomorphicTo ByteString ByteString where
  to = id

instance IsomorphicTo ByteString [Word8] where
  to = ByteString.pack

instance IsomorphicTo ByteString ByteStringLazy.ByteString where
  to = ByteStringLazy.toStrict

instance IsomorphicTo ByteString ByteStringShort.ShortByteString where
  to = ByteStringShort.fromShort

instance IsomorphicTo ByteString ByteStringBuilder.Builder where
  to = ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString

instance IsomorphicTo ByteString PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo ByteString TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString

--

instance IsomorphicTo ByteStringLazy.ByteString ByteStringLazy.ByteString where
  to = id

instance IsomorphicTo ByteStringLazy.ByteString [Word8] where
  to = ByteStringLazy.pack

instance IsomorphicTo ByteStringLazy.ByteString ByteString where
  to = ByteStringLazy.fromStrict

instance IsomorphicTo ByteStringLazy.ByteString ByteStringShort.ShortByteString where
  to = from @ByteString . to

instance IsomorphicTo ByteStringLazy.ByteString ByteStringBuilder.Builder where
  to = ByteStringBuilder.toLazyByteString

instance IsomorphicTo ByteStringLazy.ByteString PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo ByteStringLazy.ByteString TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString

--

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringShort.ShortByteString where
  to = id

instance IsomorphicTo ByteStringShort.ShortByteString [Word8] where
  to = ByteStringShort.pack

instance IsomorphicTo ByteStringShort.ShortByteString ByteString where
  to = ByteStringShort.toShort

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringLazy.ByteString where
  to = to . to @ByteString

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringBuilder.Builder where
  to = to . to @ByteStringLazy.ByteString

instance IsomorphicTo ByteStringShort.ShortByteString PrimitiveByteArray.ByteArray where
  to (PrimitiveByteArray.ByteArray array) = ByteStringShortInternal.SBS array

instance IsomorphicTo ByteStringShort.ShortByteString TextArray.Array where
  to a = ByteStringShortInternal.SBS (TextCompatArray.toUnliftedByteArray a)

--

instance IsomorphicTo ByteStringBuilder.Builder ByteStringBuilder.Builder where
  to = id

instance IsomorphicTo ByteStringBuilder.Builder [Word8] where
  to = to . to @ByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteString where
  to = ByteStringBuilder.byteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringLazy.ByteString where
  to = ByteStringBuilder.lazyByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringShort.ShortByteString where
  to = ByteStringBuilder.shortByteString

instance IsomorphicTo ByteStringBuilder.Builder PrimitiveByteArray.ByteArray where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo ByteStringBuilder.Builder TextArray.Array where
  to = to . to @ByteStringShort.ShortByteString

--

instance IsomorphicTo PrimitiveByteArray.ByteArray PrimitiveByteArray.ByteArray where
  to = id

instance IsomorphicTo PrimitiveByteArray.ByteArray [Word8] where
  to = fromList

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringShort.ShortByteString where
  to (ByteStringShortInternal.SBS array) = PrimitiveByteArray.ByteArray array

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteString where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringLazy.ByteString where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray ByteStringBuilder.Builder where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo PrimitiveByteArray.ByteArray TextArray.Array where
  to a = PrimitiveByteArray.ByteArray (TextCompatArray.toUnliftedByteArray a)

--

instance IsomorphicTo TextArray.Array [Word8] where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo TextArray.Array PrimitiveByteArray.ByteArray where
  to (PrimitiveByteArray.ByteArray arr) = TextCompatArray.fromUnliftedByteArray arr

instance IsomorphicTo TextArray.Array ByteStringShort.ShortByteString where
  to (ByteStringShortInternal.SBS arr) = TextCompatArray.fromUnliftedByteArray arr

instance IsomorphicTo TextArray.Array ByteString where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo TextArray.Array ByteStringLazy.ByteString where
  to = to . to @ByteStringShort.ShortByteString

instance IsomorphicTo TextArray.Array ByteStringBuilder.Builder where
  to = to . to @ByteStringShort.ShortByteString

--

instance IsomorphicTo (Vector a) (Vector a) where
  to = id

instance IsomorphicTo (Vector a) [a] where
  to = Vector.fromList

instance IsomorphicTo (Vector a) (Seq a) where
  to = from @[a] . to

--

instance IsomorphicTo (Seq a) (Seq a) where
  to = id

instance IsomorphicTo (Seq a) [a] where
  to = Seq.fromList

instance IsomorphicTo (Seq a) (Vector a) where
  to = from @[a] . to

--

instance IsomorphicTo (Set a) (Set a) where
  to = id

instance IsomorphicTo (Set Int) IntSet where
  to = fromList . toList

--

instance IsomorphicTo IntSet IntSet where
  to = id

instance IsomorphicTo IntSet (Set Int) where
  to = fromList . toList

--

instance IsomorphicTo (Map k v) (Map k v) where
  to = id

instance IsomorphicTo (Map Int v) (IntMap v) where
  to = fromList . toList

--

instance IsomorphicTo (IntMap a) (IntMap a) where
  to = id

instance IsomorphicTo (IntMap v) (Map Int v) where
  to = fromList . toList

--

instance IsomorphicTo (Maybe a) (Maybe a) where to = id

instance IsomorphicTo (Either a b) (Either a b) where to = id

instance IsomorphicTo (First a) (First a) where to = id

instance IsomorphicTo (Last a) (Last a) where to = id

instance IsomorphicTo (Product a) (Product a) where to = id

instance IsomorphicTo (Sum a) (Sum a) where to = id

--

instance IsomorphicTo Bool Bool where to = id

instance IsomorphicTo Char Char where to = id

instance IsomorphicTo Double Double where to = id

instance IsomorphicTo Float Float where to = id

instance IsomorphicTo Int Int where to = id

instance IsomorphicTo Int Word where to = fromIntegral

instance IsomorphicTo Int16 Int16 where to = id

instance IsomorphicTo Int16 Word16 where to = fromIntegral

instance IsomorphicTo Int32 Int32 where to = id

instance IsomorphicTo Int32 Word32 where to = fromIntegral

instance IsomorphicTo Int64 Int64 where to = id

instance IsomorphicTo Int64 Word64 where to = fromIntegral

instance IsomorphicTo Int8 Int8 where to = id

instance IsomorphicTo Int8 Word8 where to = fromIntegral

instance IsomorphicTo Integer Integer where to = id

instance IsomorphicTo Rational Rational where to = id

instance IsomorphicTo Word Int where to = fromIntegral

instance IsomorphicTo Word Word where to = id

instance IsomorphicTo Word16 Int16 where to = fromIntegral

instance IsomorphicTo Word16 Word16 where to = id

instance IsomorphicTo Word32 Int32 where to = fromIntegral

instance IsomorphicTo Word32 Word32 where to = id

instance IsomorphicTo Word64 Int64 where to = fromIntegral

instance IsomorphicTo Word64 Word64 where to = id

instance IsomorphicTo Word8 Int8 where to = fromIntegral

instance IsomorphicTo Word8 Word8 where to = id

--

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromString :: IsomorphicTo a String => String -> a
-- > fromString = from @String
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: forall a b. IsomorphicTo b a => a -> b
from = to

-- |
-- Ideally there should be a direct instance and this function
-- should merely serve as a helper for defining instances
-- by merely composing from other instances.
--
-- E.g.,
--
-- > thru @String Proxy
--
-- captures the following pattern:
--
-- > from @String . to
--
-- However it is advised to use the conversion functions directly,
-- since it makes the intent clearer and is actually shorter.
{-# INLINE thru #-}
thru :: (IsomorphicTo a b, IsomorphicTo a c) => Proxy a -> b -> c
thru proxy = from . flip asProxyTypeOf proxy . to

{-# INLINE thruString #-}
thruString :: (IsomorphicTo String a, IsomorphicTo String b) => a -> b
thruString = from @String . to

{-# INLINE thruText #-}
thruText :: (IsomorphicTo Text a, IsomorphicTo Text b) => a -> b
thruText = from @Text . to

{-# INLINE thruList #-}
thruList :: forall a f g. (IsomorphicTo [a] (f a), IsomorphicTo [a] (g a)) => f a -> g a
thruList = from @[a] . to

-- | A utility, which uses the 'Show' instance to produce a value
-- that 'String' is isomorphic to.
--
-- It lets you generalize over the functions like the following:
--
-- > showAsText :: Show a => a -> Text
-- > showAsText = showAs @Text
--
-- > showAsBuilder :: Show a => a -> Builder
-- > showAsBuilder = showAs @Builder
showAs :: forall b a. (IsomorphicTo String b, Show a) => a -> b
showAs = from . show
