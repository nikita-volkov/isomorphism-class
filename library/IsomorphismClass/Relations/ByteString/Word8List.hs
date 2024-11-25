{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteString.Word8List where

import qualified Data.ByteString
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo ByteString [Word8] where
  to = Data.ByteString.pack
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo [Word8] ByteString where
  to = Data.ByteString.unpack
  partiallyFrom = Just . to

instance IsomorphicTo ByteString [Word8]

instance IsomorphicTo [Word8] ByteString
