{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndWord8List where

import qualified Data.ByteString
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf ByteString [Word8] where
  to = Data.ByteString.pack

instance IsSubsetOf [Word8] ByteString where
  to = Data.ByteString.unpack

instance IsEqualTo ByteString [Word8]

instance IsEqualTo [Word8] ByteString
