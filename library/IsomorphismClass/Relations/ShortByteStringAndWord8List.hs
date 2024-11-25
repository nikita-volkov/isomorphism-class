{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ShortByteStringAndWord8List where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()

instance PartiallyIsomorphicTo [Word8] Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.unpack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString [Word8] where
  to = Data.ByteString.Short.pack
  partiallyFrom = Just . to

instance IsomorphicTo [Word8] Data.ByteString.Short.ShortByteString

instance IsomorphicTo Data.ByteString.Short.ShortByteString [Word8]
