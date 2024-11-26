{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndWord8List where

import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()

instance IsSome Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList

instance IsSome [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList

instance Is Data.Primitive.ByteArray.ByteArray [Word8]

instance Is [Word8] Data.Primitive.ByteArray.ByteArray
