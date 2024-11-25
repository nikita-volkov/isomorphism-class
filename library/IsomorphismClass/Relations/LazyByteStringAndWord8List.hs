{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringAndWord8List where

import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString [Word8] where
  to = Data.ByteString.Lazy.pack

instance PartiallyIsomorphicTo [Word8] Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.unpack

instance IsomorphicTo Data.ByteString.Lazy.ByteString [Word8]

instance IsomorphicTo [Word8] Data.ByteString.Lazy.ByteString
