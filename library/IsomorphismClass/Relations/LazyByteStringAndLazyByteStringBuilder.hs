{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Builder.toLazyByteString

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Builder.lazyByteString

instance IsomorphicTo Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder

instance IsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString
