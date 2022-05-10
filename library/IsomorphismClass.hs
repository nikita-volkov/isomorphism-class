-- |
-- = The ultimate solution to the Conversion Problem
--
-- By Conversion Problem we mean a repetitive chain of robotic actions one has
-- to take to translate data from one form to another without losing
-- information. This problem is often extended by packages using different
-- conventions. This library address all those problems by abstracting over
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
-- Turns out there can only be one way of defining such a conversion between
-- two types. E.g., there is only one way you can convert 'String' to 'Text'
-- in such a way that you'd be able to get back the same 'String' when
-- converting back for any input. This applies to all cases and thus makes it
-- evident to the user, what happens where he sees such a conversion, and
-- just as evident to the author how to define one.
--
-- That's why it's not just another conversion library. It is a lawful
-- abstraction, which is easy to define and infer. It helps the library
-- authors to ensure that they define something that makes sense and provides
-- insight to the users about what the conversions do. We capture this with a
-- lawful class, which ensures that there is only one proper way of defining
-- an instance for it and also accidentally provides a nice API.
--
-- Here's a few samples of what this library lets you do:
--
-- @
-- renderNameAndSurnameViaBuilder :: 'Text' -> 'Text' -> 'Text'
-- renderNameAndSurnameViaBuilder name surname =
--   'from' @t'Data.Text.Lazy.Builder.Builder' $ 'to' name <> \" \" <> 'to' surname
-- @
--
-- @
-- showText :: Show a => a -> Text
-- showText = to @Text . show
-- @
module IsomorphismClass
  ( -- * Typeclass
    IsomorphicTo (..),

    -- * Common Utilities
    showAs,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VectorGeneric
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed
import IsomorphismClass.Prelude

-- |
-- Bidirectional conversion between two types with no loss of information.
--
-- You can read the signature @IsomorphicTo a b@ as \"B is isomorphic to A\".
--
-- __Laws__
--
-- This class is lawful. The laws are:
--
-- - @'from' . 'to' = 'id'@ - Converting to a type and back from it should
-- produce a value that is identical to the original.
--
-- - @'to' . 'from' = 'id'@ - Converting from a type and back to it should too
-- produce a value that is identical to the orignal.
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
-- There's two conventions in defining instances for this class:
--
-- - Always define dual instances. If there is @IsomorphicTo String Text@,
--   then there should also be @IsomorphicTo Text String@. This serves
--   consistency, making things more predictable to the user, and it gives
--   him flexibility.
--
-- - Never define identity instances, like @IsomorphicTo Int Int@. Why?
--   Because it's useless, only adds noise and to be consistent you'll have
--   to define it for absolutely every type.
class IsomorphicTo a b where
  to :: b -> a
  from :: a -> b

--

instance IsomorphicTo String Text where
  to = Text.unpack
  from = Text.pack

instance IsomorphicTo String TextLazy.Text where
  to = TextLazy.unpack
  from = TextLazy.pack

instance IsomorphicTo String TextLazyBuilder.Builder where
  to = TextLazy.unpack . TextLazyBuilder.toLazyText
  from = TextLazyBuilder.fromString

--

instance IsomorphicTo [Word8] ByteString where
  to = ByteString.unpack
  from = ByteString.pack

instance IsomorphicTo [Word8] ByteStringLazy.ByteString where
  to = ByteStringLazy.unpack
  from = ByteStringLazy.pack

instance IsomorphicTo [Word8] ByteStringShort.ShortByteString where
  to = ByteStringShort.unpack
  from = ByteStringShort.pack

instance IsomorphicTo [Word8] ByteStringBuilder.Builder where
  to = ByteStringLazy.unpack . ByteStringBuilder.toLazyByteString
  from = ByteStringBuilder.byteString . ByteString.pack

--

instance IsomorphicTo a b => IsomorphicTo [a] [b] where
  to = fmap to
  from = fmap from

instance IsomorphicTo [a] (Vector a) where
  to = toList
  from = fromList

instance VectorUnboxed.Unbox a => IsomorphicTo [a] (VectorUnboxed.Vector a) where
  to = toList
  from = fromList

instance Storable a => IsomorphicTo [a] (VectorStorable.Vector a) where
  to = toList
  from = fromList

instance IsomorphicTo [a] (Seq a) where
  to = toList
  from = fromList

--

instance IsomorphicTo Text String where
  to = Text.pack
  from = Text.unpack

instance IsomorphicTo Text TextLazy.Text where
  to = TextLazy.toStrict
  from = TextLazy.fromStrict

instance IsomorphicTo Text TextLazyBuilder.Builder where
  to = TextLazy.toStrict . TextLazyBuilder.toLazyText
  from = TextLazyBuilder.fromText

instance IsomorphicTo Text (VectorUnboxed.Vector Char) where
  to = from @[Char] . to
  from = from @[Char] . to

instance IsomorphicTo Text (VectorStorable.Vector Char) where
  to = from @[Char] . to
  from = from @[Char] . to

--

instance IsomorphicTo TextLazy.Text String where
  to = TextLazy.pack
  from = TextLazy.unpack

instance IsomorphicTo TextLazy.Text Text where
  to = TextLazy.fromStrict
  from = TextLazy.toStrict

instance IsomorphicTo TextLazy.Text TextLazyBuilder.Builder where
  to = TextLazyBuilder.toLazyText
  from = TextLazyBuilder.fromLazyText

instance IsomorphicTo TextLazy.Text (VectorUnboxed.Vector Char) where
  to = from @[Char] . to
  from = from @[Char] . to

instance IsomorphicTo TextLazy.Text (VectorStorable.Vector Char) where
  to = from @[Char] . to
  from = from @[Char] . to

--

instance IsomorphicTo TextLazyBuilder.Builder String where
  to = from
  from = to

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = from
  from = to

instance IsomorphicTo TextLazyBuilder.Builder TextLazy.Text where
  to = from
  from = to

instance IsomorphicTo TextLazyBuilder.Builder (VectorUnboxed.Vector Char) where
  to = from @Text . to
  from = from @Text . to

instance IsomorphicTo TextLazyBuilder.Builder (VectorStorable.Vector Char) where
  to = from @Text . to
  from = from @Text . to

--

instance IsomorphicTo ByteString [Word8] where
  to = from
  from = to

instance IsomorphicTo ByteString ByteStringLazy.ByteString where
  to = ByteStringLazy.toStrict
  from = ByteStringLazy.fromStrict

instance IsomorphicTo ByteString ByteStringShort.ShortByteString where
  to = ByteStringShort.fromShort
  from = ByteStringShort.toShort

instance IsomorphicTo ByteString ByteStringBuilder.Builder where
  to = ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString
  from = ByteStringBuilder.byteString

instance IsomorphicTo ByteString (VectorUnboxed.Vector Word8) where
  to = ByteString.pack . VectorUnboxed.toList
  from = VectorUnboxed.fromList . ByteString.unpack

instance IsomorphicTo ByteString (VectorStorable.Vector Word8) where
  to = ByteString.pack . VectorStorable.toList
  from = VectorStorable.fromList . ByteString.unpack

--

instance IsomorphicTo ByteStringLazy.ByteString [Word8] where
  to = from
  from = to

instance IsomorphicTo ByteStringLazy.ByteString ByteString where
  to = from
  from = to

instance IsomorphicTo ByteStringLazy.ByteString ByteStringShort.ShortByteString where
  to = from @ByteString . to
  from = from @ByteString . to

instance IsomorphicTo ByteStringLazy.ByteString ByteStringBuilder.Builder where
  to = ByteStringBuilder.toLazyByteString
  from = ByteStringBuilder.lazyByteString

instance IsomorphicTo ByteStringLazy.ByteString (VectorUnboxed.Vector Word8) where
  to = ByteStringLazy.pack . VectorUnboxed.toList
  from = VectorUnboxed.fromList . ByteStringLazy.unpack

instance IsomorphicTo ByteStringLazy.ByteString (VectorStorable.Vector Word8) where
  to = ByteStringLazy.pack . VectorStorable.toList
  from = VectorStorable.fromList . ByteStringLazy.unpack

--

instance IsomorphicTo ByteStringShort.ShortByteString [Word8] where
  to = from
  from = to

instance IsomorphicTo ByteStringShort.ShortByteString ByteString where
  to = from
  from = to

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringLazy.ByteString where
  to = from
  from = to

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringBuilder.Builder where
  to = from
  from = to

--

instance IsomorphicTo ByteStringBuilder.Builder [Word8] where
  to = from
  from = to

instance IsomorphicTo ByteStringBuilder.Builder ByteString where
  to = from
  from = to

instance IsomorphicTo ByteStringBuilder.Builder ByteStringLazy.ByteString where
  to = from
  from = to

instance IsomorphicTo ByteStringBuilder.Builder ByteStringShort.ShortByteString where
  to = ByteStringBuilder.shortByteString
  from = from @ByteString . to

instance IsomorphicTo ByteStringBuilder.Builder (VectorUnboxed.Vector Word8) where
  to = from @ByteString . to
  from = from @ByteString . to

instance IsomorphicTo ByteStringBuilder.Builder (VectorStorable.Vector Word8) where
  to = from @ByteString . to
  from = from @ByteString . to

--

instance IsomorphicTo a b => IsomorphicTo (Vector a) (Vector b) where
  to = fmap to
  from = fmap from

instance IsomorphicTo (Vector a) [a] where
  to = from @[a]
  from = to @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (Vector a) (VectorUnboxed.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR
  from = VectorGeneric.unstreamR . VectorGeneric.streamR

instance Storable a => IsomorphicTo (Vector a) (VectorStorable.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR
  from = VectorGeneric.unstreamR . VectorGeneric.streamR

instance IsomorphicTo (Vector a) (Seq a) where
  to = from @[a] . to
  from = from @[a] . to

--

instance (IsomorphicTo a b, VectorUnboxed.Unbox a, VectorUnboxed.Unbox b) => IsomorphicTo (VectorUnboxed.Vector a) (VectorUnboxed.Vector b) where
  to = VectorUnboxed.map to
  from = VectorUnboxed.map from

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) [a] where
  to = from @[a]
  from = to @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Vector a) where
  to = from @(Vector a)
  from = to @(Vector a)

instance (VectorUnboxed.Unbox a, Storable a) => IsomorphicTo (VectorUnboxed.Vector a) (VectorStorable.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR
  from = VectorGeneric.unstreamR . VectorGeneric.streamR

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Seq a) where
  to = from @[a] . to
  from = from @[a] . to

instance IsomorphicTo (VectorUnboxed.Vector Char) Text where
  to = from
  from = to

--

instance (IsomorphicTo a b, Storable a, Storable b) => IsomorphicTo (VectorStorable.Vector a) (VectorStorable.Vector b) where
  to = VectorStorable.map to
  from = VectorStorable.map from

instance Storable a => IsomorphicTo (VectorStorable.Vector a) [a] where
  to = from @[a]
  from = to @[a]

instance Storable a => IsomorphicTo (VectorStorable.Vector a) (Vector a) where
  to = from @(Vector a)
  from = to @(Vector a)

instance (VectorUnboxed.Unbox a, Storable a) => IsomorphicTo (VectorStorable.Vector a) (VectorUnboxed.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR
  from = VectorGeneric.unstreamR . VectorGeneric.streamR

instance Storable a => IsomorphicTo (VectorStorable.Vector a) (Seq a) where
  to = from @[a] . to
  from = from @[a] . to

instance IsomorphicTo (VectorStorable.Vector Char) Text where
  to = from
  from = to

--

instance IsomorphicTo a b => IsomorphicTo (Seq a) (Seq b) where
  to = fmap to
  from = fmap from

instance IsomorphicTo (Seq a) [a] where
  to = from @[a]
  from = to @[a]

instance IsomorphicTo (Seq a) (Vector a) where
  to = from @[a] . to
  from = from @[a] . to

instance VectorUnboxed.Unbox a => IsomorphicTo (Seq a) (VectorUnboxed.Vector a) where
  to = from @[a] . to
  from = from @[a] . to

instance Storable a => IsomorphicTo (Seq a) (VectorStorable.Vector a) where
  to = from @[a] . to
  from = from @[a] . to

--

instance (IsomorphicTo a b, Ord a, Ord b) => IsomorphicTo (Set a) (Set b) where
  to = Set.map to
  from = Set.map from

instance (Hashable a, Ord a) => IsomorphicTo (Set a) (HashSet a) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (Set Int) IntSet where
  to = fromList . toList
  from = fromList . toList

--

instance (IsomorphicTo a b, Eq a, Hashable a, Eq b, Hashable b) => IsomorphicTo (HashSet a) (HashSet b) where
  to = HashSet.map to
  from = HashSet.map from

instance (Hashable a, Ord a) => IsomorphicTo (HashSet a) (Set a) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (HashSet Int) IntSet where
  to = fromList . toList
  from = fromList . toList

--

instance IsomorphicTo IntSet (Set Int) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo IntSet (HashSet Int) where
  to = fromList . toList
  from = fromList . toList

--

instance (Hashable k, Ord k) => IsomorphicTo (HashMap k v) (Map k v) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (HashMap Int v) (IntMap v) where
  to = fromList . toList
  from = fromList . toList

--

instance (Hashable k, Ord k) => IsomorphicTo (Map k v) (HashMap k v) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (Map Int v) (IntMap v) where
  to = fromList . toList
  from = fromList . toList

--

instance IsomorphicTo (IntMap v) (HashMap Int v) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (IntMap v) (Map Int v) where
  to = fromList . toList
  from = fromList . toList

--

instance IsomorphicTo Int8 Word8 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Int16 Word16 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Int32 Word32 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Int64 Word64 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Int Word where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Word8 Int8 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Word16 Int16 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Word32 Int32 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Word64 Int64 where
  to = fromIntegral
  from = fromIntegral

instance IsomorphicTo Word Int where
  to = fromIntegral
  from = fromIntegral

--

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
showAs :: (IsomorphicTo b String, Show a) => a -> b
showAs = to . show
