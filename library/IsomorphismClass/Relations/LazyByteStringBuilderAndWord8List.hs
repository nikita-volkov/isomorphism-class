{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringBuilderAndWord8List where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Data.ByteString.Builder.Builder [Word8] where
  to = Data.ByteString.Builder.lazyByteString . Data.ByteString.Lazy.pack

instance IsomorphicToSubsetOf [Word8] Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.unpack . Data.ByteString.Builder.toLazyByteString

instance IsomorphicTo Data.ByteString.Builder.Builder [Word8]

instance IsomorphicTo [Word8] Data.ByteString.Builder.Builder
