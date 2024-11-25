{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArray.Word8List where

import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArray.ShortByteString ()

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList
  partiallyFrom = Just . to

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray [Word8]

instance IsomorphicTo [Word8] Data.Primitive.ByteArray.ByteArray