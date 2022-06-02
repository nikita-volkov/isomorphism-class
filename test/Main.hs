module Main where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Short as ByteStringShort
import qualified Data.Primitive.ByteArray as PrimitiveByteArray
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VectorGeneric
import IsomorphismClass
import Rebase.Prelude
import Test.ExtraInstances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

main = defaultMain allTests

allTests =
  testGroup "All" $
    [ testPair @String @Text Proxy Proxy,
      testPair @String @TextLazy.Text Proxy Proxy,
      testPair @String @TextLazyBuilder.Builder Proxy Proxy,
      testPair @[Word8] @ByteString Proxy Proxy,
      testPair @[Word8] @ByteStringLazy.ByteString Proxy Proxy,
      testPair @[Word8] @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @[Word8] @ByteStringBuilder.Builder Proxy Proxy,
      testPair @[Word8] @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @[Word8] @(Vector Word8) Proxy Proxy,
      testPair @[Word8] @(Seq Word8) Proxy Proxy,
      testPair @Text @String Proxy Proxy,
      testPair @Text @TextLazy.Text Proxy Proxy,
      testPair @Text @TextLazyBuilder.Builder Proxy Proxy,
      testPair @TextLazy.Text @String Proxy Proxy,
      testPair @TextLazy.Text @Text Proxy Proxy,
      testPair @TextLazy.Text @TextLazyBuilder.Builder Proxy Proxy,
      testPair @TextLazyBuilder.Builder @String Proxy Proxy,
      testPair @TextLazyBuilder.Builder @Text Proxy Proxy,
      testPair @TextLazyBuilder.Builder @TextLazy.Text Proxy Proxy,
      testPair @ByteString @[Word8] Proxy Proxy,
      testPair @ByteString @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteString @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringLazy.ByteString @[Word8] Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteString Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteStringLazy.ByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteStringLazy.ByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @[Word8] Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteString Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @ByteStringBuilder.Builder Proxy Proxy,
      testPair @ByteStringShort.ShortByteString @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @ByteStringBuilder.Builder @[Word8] Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteStringLazy.ByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @ByteStringBuilder.Builder @PrimitiveByteArray.ByteArray Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @[Word8] Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringShort.ShortByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringLazy.ByteString Proxy Proxy,
      testPair @PrimitiveByteArray.ByteArray @ByteStringBuilder.Builder Proxy Proxy,
      testPair @(Vector Word8) @[Word8] Proxy Proxy,
      testPair @(Vector Word8) @(Seq Word8) Proxy Proxy,
      testPair @(Seq Word8) @[Word8] Proxy Proxy,
      testPair @(Seq Word8) @(Vector Word8) Proxy Proxy,
      testPair @(Set Int) @IntSet Proxy Proxy,
      testPair @IntSet @(Set Int) Proxy Proxy,
      testPair @(Map Int Word8) @(IntMap Word8) Proxy Proxy,
      testPair @(IntMap Word8) @(Map Int Word8) Proxy Proxy,
      testPair @Int @Word Proxy Proxy,
      testPair @Int16 @Word16 Proxy Proxy,
      testPair @Int32 @Word32 Proxy Proxy,
      testPair @Int64 @Word64 Proxy Proxy,
      testPair @Int8 @Word8 Proxy Proxy,
      testPair @Word @Int Proxy Proxy,
      testPair @Word16 @Int16 Proxy Proxy,
      testPair @Word32 @Int32 Proxy Proxy,
      testPair @Word64 @Int64 Proxy Proxy,
      testPair @Word8 @Int8 Proxy Proxy
    ]

testPair :: forall a b. (IsomorphicTo a b, Eq a, Arbitrary a, Show a, Typeable a, Typeable b) => Proxy a -> Proxy b -> TestTree
testPair _ _ =
  testProperty name $ \a ->
    a === from @b (from @a a)
  where
    name = show (typeOf (undefined :: a)) <> "/" <> show (typeOf (undefined :: b))
