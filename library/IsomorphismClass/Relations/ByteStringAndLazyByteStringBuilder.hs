{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString

instance IsSubsetOf Data.ByteString.Builder.Builder ByteString where
  to = Data.ByteString.Builder.byteString

instance IsEqualTo Data.ByteString.Builder.Builder ByteString

instance IsEqualTo ByteString Data.ByteString.Builder.Builder
