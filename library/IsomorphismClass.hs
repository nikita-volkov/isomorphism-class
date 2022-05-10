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
    from,

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
-- As you can see the typeclass has a dependency on itself with arguments
-- flipped. Thus we state that mappings exist in both directions. So for each
-- pair of isomorphic types (say, A and B) the compiler will require you have
-- to define two instances, namely: @IsomorphicTo A B@ and @IsomorphicTo B A@.
--
-- The current policy in regards to identity instances (from A to A) is not to
-- define them. Until proven otherwise they don't seem to be particularly
-- useful and if we were to define them, absolutely every type would have an
-- instance, so it seems like a lot of noise. IOW, please avoid defining
-- identity instances.
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

--

instance IsomorphicTo a b => IsomorphicTo [a] [b] where
  to = fmap to

instance IsomorphicTo [a] (Vector a) where
  to = toList

instance VectorUnboxed.Unbox a => IsomorphicTo [a] (VectorUnboxed.Vector a) where
  to = toList

instance Storable a => IsomorphicTo [a] (VectorStorable.Vector a) where
  to = toList

instance IsomorphicTo [a] (Seq a) where
  to = toList

--

instance IsomorphicTo Text String where
  to = Text.pack

instance IsomorphicTo Text TextLazy.Text where
  to = TextLazy.toStrict

instance IsomorphicTo Text TextLazyBuilder.Builder where
  to = TextLazy.toStrict . TextLazyBuilder.toLazyText

instance IsomorphicTo Text (VectorUnboxed.Vector Char) where
  to = from @[Char] . to

instance IsomorphicTo Text (VectorStorable.Vector Char) where
  to = from @[Char] . to

--

instance IsomorphicTo TextLazy.Text String where
  to = TextLazy.pack

instance IsomorphicTo TextLazy.Text Text where
  to = TextLazy.fromStrict

instance IsomorphicTo TextLazy.Text TextLazyBuilder.Builder where
  to = TextLazyBuilder.toLazyText

instance IsomorphicTo TextLazy.Text (VectorUnboxed.Vector Char) where
  to = from @[Char] . to

instance IsomorphicTo TextLazy.Text (VectorStorable.Vector Char) where
  to = from @[Char] . to

--

instance IsomorphicTo TextLazyBuilder.Builder String where
  to = TextLazyBuilder.fromString

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = TextLazyBuilder.fromText

instance IsomorphicTo TextLazyBuilder.Builder TextLazy.Text where
  to = TextLazyBuilder.fromLazyText

instance IsomorphicTo TextLazyBuilder.Builder (VectorUnboxed.Vector Char) where
  to = from @Text . to

instance IsomorphicTo TextLazyBuilder.Builder (VectorStorable.Vector Char) where
  to = from @Text . to

--

instance IsomorphicTo ByteString [Word8] where
  to = ByteString.pack

instance IsomorphicTo ByteString ByteStringLazy.ByteString where
  to = ByteStringLazy.toStrict

instance IsomorphicTo ByteString ByteStringShort.ShortByteString where
  to = ByteStringShort.fromShort

instance IsomorphicTo ByteString ByteStringBuilder.Builder where
  to = ByteStringLazy.toStrict . ByteStringBuilder.toLazyByteString

instance IsomorphicTo ByteString (VectorUnboxed.Vector Word8) where
  to = ByteString.pack . VectorUnboxed.toList

instance IsomorphicTo ByteString (VectorStorable.Vector Word8) where
  to = ByteString.pack . VectorStorable.toList

--

instance IsomorphicTo ByteStringLazy.ByteString [Word8] where
  to = ByteStringLazy.pack

instance IsomorphicTo ByteStringLazy.ByteString ByteString where
  to = ByteStringLazy.fromStrict

instance IsomorphicTo ByteStringLazy.ByteString ByteStringShort.ShortByteString where
  to = from @ByteString . to

instance IsomorphicTo ByteStringLazy.ByteString ByteStringBuilder.Builder where
  to = ByteStringBuilder.toLazyByteString

instance IsomorphicTo ByteStringLazy.ByteString (VectorUnboxed.Vector Word8) where
  to = ByteStringLazy.pack . VectorUnboxed.toList

instance IsomorphicTo ByteStringLazy.ByteString (VectorStorable.Vector Word8) where
  to = ByteStringLazy.pack . VectorStorable.toList

--

instance IsomorphicTo ByteStringShort.ShortByteString [Word8] where
  to = ByteStringShort.pack

instance IsomorphicTo ByteStringShort.ShortByteString ByteString where
  to = ByteStringShort.toShort

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringLazy.ByteString where
  to = to . to @ByteString

instance IsomorphicTo ByteStringShort.ShortByteString ByteStringBuilder.Builder where
  to = to . to @ByteStringLazy.ByteString

instance IsomorphicTo ByteStringShort.ShortByteString (VectorUnboxed.Vector Word8) where
  to = to . to @ByteString

instance IsomorphicTo ByteStringShort.ShortByteString (VectorStorable.Vector Word8) where
  to = to . to @ByteString

--

instance IsomorphicTo ByteStringBuilder.Builder [Word8] where
  to = to . to @ByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteString where
  to = ByteStringBuilder.byteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringLazy.ByteString where
  to = ByteStringBuilder.lazyByteString

instance IsomorphicTo ByteStringBuilder.Builder ByteStringShort.ShortByteString where
  to = ByteStringBuilder.shortByteString

instance IsomorphicTo ByteStringBuilder.Builder (VectorUnboxed.Vector Word8) where
  to = from @ByteString . to

instance IsomorphicTo ByteStringBuilder.Builder (VectorStorable.Vector Word8) where
  to = from @ByteString . to

--

instance IsomorphicTo a b => IsomorphicTo (Vector a) (Vector b) where
  to = fmap to

instance IsomorphicTo (Vector a) [a] where
  to = from @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (Vector a) (VectorUnboxed.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR

instance Storable a => IsomorphicTo (Vector a) (VectorStorable.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR

instance IsomorphicTo (Vector a) (Seq a) where
  to = from @[a] . to

--

instance (IsomorphicTo a b, VectorUnboxed.Unbox a, VectorUnboxed.Unbox b) => IsomorphicTo (VectorUnboxed.Vector a) (VectorUnboxed.Vector b) where
  to = VectorUnboxed.map to

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) [a] where
  to = from @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Vector a) where
  to = from @(Vector a)

instance (VectorUnboxed.Unbox a, Storable a) => IsomorphicTo (VectorUnboxed.Vector a) (VectorStorable.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Seq a) where
  to = from @[a] . to

instance IsomorphicTo (VectorUnboxed.Vector Char) Text where
  to = from @[Char] . to

instance IsomorphicTo (VectorUnboxed.Vector Char) TextLazy.Text where
  to = from @[Char] . to

instance IsomorphicTo (VectorUnboxed.Vector Char) TextLazyBuilder.Builder where
  to = thru @TextLazy.Text Proxy

instance IsomorphicTo (VectorUnboxed.Vector Word8) ByteString where
  to = VectorUnboxed.fromList . ByteString.unpack

instance IsomorphicTo (VectorUnboxed.Vector Word8) ByteStringLazy.ByteString where
  to = VectorUnboxed.fromList . ByteStringLazy.unpack

instance IsomorphicTo (VectorUnboxed.Vector Word8) ByteStringShort.ShortByteString where
  to = from @ByteString . to

instance IsomorphicTo (VectorUnboxed.Vector Word8) ByteStringBuilder.Builder where
  to = from @ByteString . to

--

instance (IsomorphicTo a b, Storable a, Storable b) => IsomorphicTo (VectorStorable.Vector a) (VectorStorable.Vector b) where
  to = VectorStorable.map to

instance Storable a => IsomorphicTo (VectorStorable.Vector a) [a] where
  to = from @[a]

instance Storable a => IsomorphicTo (VectorStorable.Vector a) (Vector a) where
  to = from @(Vector a)

instance (VectorUnboxed.Unbox a, Storable a) => IsomorphicTo (VectorStorable.Vector a) (VectorUnboxed.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR

instance Storable a => IsomorphicTo (VectorStorable.Vector a) (Seq a) where
  to = from @[a] . to

instance IsomorphicTo (VectorStorable.Vector Char) Text where
  to = from @[Char] . to

instance IsomorphicTo (VectorStorable.Vector Char) TextLazy.Text where
  to = from @[Char] . to

instance IsomorphicTo (VectorStorable.Vector Char) TextLazyBuilder.Builder where
  to = thru @TextLazy.Text Proxy

instance IsomorphicTo (VectorStorable.Vector Word8) ByteString where
  to = VectorStorable.fromList . ByteString.unpack

instance IsomorphicTo (VectorStorable.Vector Word8) ByteStringLazy.ByteString where
  to = VectorStorable.fromList . ByteStringLazy.unpack

instance IsomorphicTo (VectorStorable.Vector Word8) ByteStringBuilder.Builder where
  to = from @ByteString . to

instance IsomorphicTo (VectorStorable.Vector Word8) ByteStringShort.ShortByteString where
  to = from @ByteString . to

--

instance IsomorphicTo a b => IsomorphicTo (Seq a) (Seq b) where
  to = fmap to

instance IsomorphicTo (Seq a) [a] where
  to = from @[a]

instance IsomorphicTo (Seq a) (Vector a) where
  to = from @[a] . to

instance VectorUnboxed.Unbox a => IsomorphicTo (Seq a) (VectorUnboxed.Vector a) where
  to = from @[a] . to

instance Storable a => IsomorphicTo (Seq a) (VectorStorable.Vector a) where
  to = from @[a] . to

--

instance (IsomorphicTo a b, Ord a, Ord b) => IsomorphicTo (Set a) (Set b) where
  to = Set.map to

instance (Hashable a, Ord a) => IsomorphicTo (Set a) (HashSet a) where
  to = fromList . toList

instance IsomorphicTo (Set Int) IntSet where
  to = fromList . toList

--

instance (IsomorphicTo a b, Eq a, Hashable a, Eq b, Hashable b) => IsomorphicTo (HashSet a) (HashSet b) where
  to = HashSet.map to

instance (Hashable a, Ord a) => IsomorphicTo (HashSet a) (Set a) where
  to = fromList . toList

instance IsomorphicTo (HashSet Int) IntSet where
  to = fromList . toList

--

instance IsomorphicTo IntSet (Set Int) where
  to = fromList . toList

instance IsomorphicTo IntSet (HashSet Int) where
  to = fromList . toList

--

instance (Hashable k, Ord k) => IsomorphicTo (HashMap k v) (Map k v) where
  to = fromList . toList

instance IsomorphicTo (HashMap Int v) (IntMap v) where
  to = fromList . toList

--

instance (Hashable k, Ord k) => IsomorphicTo (Map k v) (HashMap k v) where
  to = fromList . toList

instance IsomorphicTo (Map Int v) (IntMap v) where
  to = fromList . toList

--

instance IsomorphicTo (IntMap v) (HashMap Int v) where
  to = fromList . toList

instance IsomorphicTo (IntMap v) (Map Int v) where
  to = fromList . toList

--

instance IsomorphicTo Int8 Word8 where
  to = fromIntegral

instance IsomorphicTo Int16 Word16 where
  to = fromIntegral

instance IsomorphicTo Int32 Word32 where
  to = fromIntegral

instance IsomorphicTo Int64 Word64 where
  to = fromIntegral

instance IsomorphicTo Int Word where
  to = fromIntegral

instance IsomorphicTo Word8 Int8 where
  to = fromIntegral

instance IsomorphicTo Word16 Int16 where
  to = fromIntegral

instance IsomorphicTo Word32 Int32 where
  to = fromIntegral

instance IsomorphicTo Word64 Int64 where
  to = fromIntegral

instance IsomorphicTo Word Int where
  to = fromIntegral

--

-- |
-- 'to' in reverse direction.
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
showAs :: (IsomorphicTo b String, Show a) => a -> b
showAs = to . show
