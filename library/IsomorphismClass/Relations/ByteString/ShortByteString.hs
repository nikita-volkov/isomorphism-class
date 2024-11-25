{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteString.ShortByteString where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort
  partiallyFrom = Just . to

instance IsomorphicTo ByteString Data.ByteString.Short.ShortByteString

instance IsomorphicTo Data.ByteString.Short.ShortByteString ByteString
