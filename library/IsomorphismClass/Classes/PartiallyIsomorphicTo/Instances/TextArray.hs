{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.TextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Primitive.ByteArray
import Data.Text.Array
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance PartiallyIsomorphicTo Array Array where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo Array [Word8] where
  to = to . Data.ByteString.Short.pack
  partiallyFrom = fmap Data.ByteString.Short.unpack . partiallyFrom

instance PartiallyIsomorphicTo Array Data.Primitive.ByteArray.ByteArray where
  to = IsomorphismClass.TextCompat.Array.fromByteArray
  partiallyFrom = Just . IsomorphismClass.TextCompat.Array.toByteArray

instance PartiallyIsomorphicTo Array Data.ByteString.Short.ShortByteString where
  to = IsomorphismClass.TextCompat.Array.fromShortByteString
  partiallyFrom = Just . IsomorphismClass.TextCompat.Array.toShortByteString

instance PartiallyIsomorphicTo Array ByteString where
  to = to . Data.ByteString.Short.toShort
  partiallyFrom = fmap Data.ByteString.Short.fromShort . partiallyFrom

instance PartiallyIsomorphicTo Array Data.ByteString.Lazy.ByteString where
  to = to . Data.ByteString.Lazy.toStrict
  partiallyFrom = fmap Data.ByteString.Lazy.fromStrict . partiallyFrom

instance PartiallyIsomorphicTo Array Data.ByteString.Builder.Builder where
  to = to . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = fmap Data.ByteString.Builder.shortByteString . partiallyFrom

#endif
