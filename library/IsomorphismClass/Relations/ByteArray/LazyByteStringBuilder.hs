{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArray.LazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArray.ShortByteString ()
import IsomorphismClass.Relations.LazyByteStringBuilder.ShortByteString ()

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance IsomorphicTo Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray
