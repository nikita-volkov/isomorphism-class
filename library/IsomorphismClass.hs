-- |
-- The ultimate solution to the Conversion Problem.
--
-- By Conversion Problem we mean occasionally having to go thru a repetitive
-- chain of brainless actions to be able to go from one representation of some
-- information into its other form in such a way that no information gets lost?
-- It's likely that you do that hourly.
--
-- How often do you use the `toList` function? How about importing `Data.Text`
-- only to be able to call its 'unpack'? How about going thru the always
-- fun sequence of importing `Data.Text.Lazy.Builder` only to be able to call its
-- `toLazyText` and then importing `Data.Text.Lazy` only to call its
-- `toStrict`?
--
-- Those all are instances of one pattern. They are conversions of
-- representations of data, which lose no information. The loss of no
-- information can be proven by being able to restore data identical to the
-- original from its transformed representation.
--
-- Turns out there can only be one way of defining such an instance. So when it
-- doesn't make it evident what happens during conversions, like from `String`
-- to `Text` and back, it at least makes it very easy to remember what each
-- instance is doing.
--
-- Why another conversion library? No conversion library has become standard for
-- a reason. I think it's because they are lawless. Which means that there are
-- millions of ways of defining a lawless conversion. No help for library
-- authors to ensure whether they define something that makes sense. And no
-- insight for the users about what the conversions do.
--
-- Here's an example of what this library lets you do:
--
-- >renderNameAndSurname :: Text -> Text -> Text
-- >renderNameAndSurname name surname =
-- >  from @Builder $ to name <> " " <> to surname
module IsomorphismClass
  ( -- * Typeclass
    IsomorphicTo (..),
  )
where

import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VectorGeneric
import qualified Data.Vector.Unboxed as VectorUnboxed
import IsomorphismClass.Prelude

-- |
-- Bidirectional conversion between two types with no loss of information.
--
-- You can read the signature @IsomorphicTo a b@ as \"B is isomorphic to A\".
--
-- This class is lawful. The laws are:
--
-- - @'from' . 'to' = 'id'@ - Converting to a type and back from it should
-- produce a value that is identical to the original.
--
-- - @'to' . 'from' = 'id'@ - Converting from a type and back to it should too
-- produce a value that is identical to the orignal.
--
-- This class is particularly easy to use in combination with the @TypeApplications@ extension
-- making it clear to the reader what sort of conversion he sees. E.g.,
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
class IsomorphicTo a b where
  to :: b -> a
  from :: a -> b

instance IsomorphicTo String String where
  to = id
  from = id

instance IsomorphicTo String Text where
  to = Text.unpack
  from = Text.pack

instance IsomorphicTo String TextLazy.Text where
  to = TextLazy.unpack
  from = TextLazy.pack

instance IsomorphicTo String TextLazyBuilder.Builder where
  to = TextLazy.unpack . TextLazyBuilder.toLazyText
  from = TextLazyBuilder.fromString

instance IsomorphicTo Text Text where
  to = id
  from = id

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

instance IsomorphicTo TextLazy.Text TextLazy.Text where
  to = id
  from = id

instance IsomorphicTo TextLazy.Text String where
  to = fromString
  from = TextLazy.unpack

instance IsomorphicTo TextLazy.Text Text where
  to = TextLazy.fromStrict
  from = TextLazy.toStrict

instance IsomorphicTo TextLazyBuilder.Builder TextLazyBuilder.Builder where
  to = id
  from = id

instance IsomorphicTo TextLazyBuilder.Builder String where
  to = fromString
  from = to . to @Text

instance IsomorphicTo TextLazyBuilder.Builder Text where
  to = TextLazyBuilder.fromText
  from = TextLazy.toStrict . TextLazyBuilder.toLazyText

instance IsomorphicTo a b => IsomorphicTo [a] [b] where
  to = fmap to
  from = fmap from

instance IsomorphicTo [a] (Vector a) where
  to = toList
  from = fromList

instance VectorUnboxed.Unbox a => IsomorphicTo [a] (VectorUnboxed.Vector a) where
  to = toList
  from = fromList

instance IsomorphicTo [a] (Seq a) where
  to = toList
  from = fromList

instance IsomorphicTo a b => IsomorphicTo (Vector a) (Vector b) where
  to = fmap to
  from = fmap from

instance IsomorphicTo (Vector a) [a] where
  to = from @[a]
  from = to @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (Vector a) (VectorUnboxed.Vector a) where
  to = VectorGeneric.unstreamR . VectorGeneric.streamR
  from = VectorGeneric.unstreamR . VectorGeneric.streamR

instance IsomorphicTo (Vector a) (Seq a) where
  to = from @[a] . to
  from = from @[a] . to

instance (IsomorphicTo a b, VectorUnboxed.Unbox a, VectorUnboxed.Unbox b) => IsomorphicTo (VectorUnboxed.Vector a) (VectorUnboxed.Vector b) where
  to = VectorUnboxed.map to
  from = VectorUnboxed.map from

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) [a] where
  to = from @[a]
  from = to @[a]

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Vector a) where
  to = from @(Vector a)
  from = to @(Vector a)

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) (Seq a) where
  to = from @[a] . to
  from = from @[a] . to

instance IsomorphicTo (VectorUnboxed.Vector Char) Text where
  to = from @[Char] . to
  from = from @[Char] . to

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

instance (IsomorphicTo a b, Ord a, Ord b) => IsomorphicTo (Set a) (Set b) where
  to = Set.map to
  from = Set.map from

instance (Hashable a, Ord a) => IsomorphicTo (Set a) (HashSet a) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (Set Int) IntSet where
  to = fromList . toList
  from = fromList . toList

instance (IsomorphicTo a b, Eq a, Hashable a, Eq b, Hashable b) => IsomorphicTo (HashSet a) (HashSet b) where
  to = HashSet.map to
  from = HashSet.map from

instance (Hashable a, Ord a) => IsomorphicTo (HashSet a) (Set a) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo (HashSet Int) IntSet where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo IntSet (Set Int) where
  to = fromList . toList
  from = fromList . toList

instance IsomorphicTo IntSet (HashSet Int) where
  to = fromList . toList
  from = fromList . toList

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
