{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringAndShortByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Lazy.fromStrict . Data.ByteString.Short.fromShort
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict
  partiallyFrom = Just . to

instance IsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString

instance IsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString
