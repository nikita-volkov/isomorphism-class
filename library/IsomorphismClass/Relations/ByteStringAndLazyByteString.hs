{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndLazyByteString where

import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict

instance IsomorphicToSubsetOf Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict

instance IsomorphicTo ByteString Data.ByteString.Lazy.ByteString

instance IsomorphicTo Data.ByteString.Lazy.ByteString ByteString
