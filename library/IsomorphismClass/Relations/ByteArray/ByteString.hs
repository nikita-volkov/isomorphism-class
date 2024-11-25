{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArray.ByteString where

import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArray.ShortByteString ()
import IsomorphismClass.Relations.ByteString.ShortByteString ()

instance PartiallyIsomorphicTo Data.Primitive.ByteArray.ByteArray ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString
  partiallyFrom = Just . to

instance IsomorphicTo Data.Primitive.ByteArray.ByteArray ByteString

instance IsomorphicTo ByteString Data.Primitive.ByteArray.ByteArray
