{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndLazyByteString where

import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict
  partiallyFrom = Just . to

instance IsomorphicTo ByteString Data.ByteString.Lazy.ByteString

instance IsomorphicTo Data.ByteString.Lazy.ByteString ByteString
