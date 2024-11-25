{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArray.LazyByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArray.ShortByteString ()
import IsomorphismClass.Relations.LazyByteString.ShortByteString ()

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance IsomorphicTo Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray
