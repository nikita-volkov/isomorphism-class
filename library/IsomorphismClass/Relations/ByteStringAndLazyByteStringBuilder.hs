{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder ByteString where
  to = Data.ByteString.Builder.byteString
  partiallyFrom = Just . to

instance IsomorphicTo Data.ByteString.Builder.Builder ByteString

instance IsomorphicTo ByteString Data.ByteString.Builder.Builder
