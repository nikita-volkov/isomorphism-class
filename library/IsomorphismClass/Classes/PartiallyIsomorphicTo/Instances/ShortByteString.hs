{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.ShortByteString where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Short.ShortByteString where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString [Word8] where
  to = Data.ByteString.Short.pack
  partiallyFrom = Just . Data.ByteString.Short.unpack

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort
  partiallyFrom = Just . Data.ByteString.Short.fromShort

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict
  partiallyFrom = Just . Data.ByteString.Lazy.fromStrict . Data.ByteString.Short.fromShort

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . Data.ByteString.Builder.shortByteString

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.Primitive.ByteArray.ByteArray where
  to (Data.Primitive.ByteArray.ByteArray array) =
    Data.ByteString.Short.Internal.SBS array
  partiallyFrom (Data.ByteString.Short.Internal.SBS array) =
    Just (Data.Primitive.ByteArray.ByteArray array)
