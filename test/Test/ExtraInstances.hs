{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ExtraInstances () where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Data.Vector.Generic as VectorGeneric
import qualified Data.Vector.Primitive as VectorPrimitive
import Rebase.Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance (Arbitrary a, VectorPrimitive.Prim a) => Arbitrary (VectorPrimitive.Vector a) where
  arbitrary = arbitraryVector
  shrink = shrinkVector

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary = TextLazyBuilder.fromText <$> arbitrary
  shrink = shrinkMap TextLazyBuilder.fromLazyText TextLazyBuilder.toLazyText

instance Eq ByteStringBuilder.Builder where
  (==) = on (==) ByteStringBuilder.toLazyByteString

instance Arbitrary ByteStringBuilder.Builder where
  arbitrary = ByteStringBuilder.byteString <$> arbitrary
  shrink = shrinkMap ByteStringBuilder.lazyByteString ByteStringBuilder.toLazyByteString

arbitraryVector :: (VectorGeneric.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = VectorGeneric.fromList <$> arbitrary

shrinkVector :: (VectorGeneric.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap VectorGeneric.fromList . shrink . VectorGeneric.toList
