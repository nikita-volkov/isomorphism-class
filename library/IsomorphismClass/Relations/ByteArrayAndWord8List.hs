{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndWord8List where

import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()

instance IsomorphicToSubsetOf Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList

instance IsomorphicToSubsetOf [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray [Word8]

instance IsomorphicTo [Word8] Data.Primitive.ByteArray.ByteArray
