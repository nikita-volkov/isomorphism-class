{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndWord8List where

import qualified Data.ByteString
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf ByteString [Word8] where
  to = Data.ByteString.pack

instance IsomorphicToSubsetOf [Word8] ByteString where
  to = Data.ByteString.unpack

instance IsomorphicTo ByteString [Word8]

instance IsomorphicTo [Word8] ByteString
