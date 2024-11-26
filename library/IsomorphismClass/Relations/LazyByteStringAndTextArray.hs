{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsSubsetOf Data.ByteString.Lazy.ByteString Data.Text.Array.Array where
  to =
    Data.ByteString.Lazy.fromStrict
      . Data.ByteString.Short.fromShort
      . IsomorphismClass.TextCompat.Array.toShortByteString

instance IsSubsetOf Data.Text.Array.Array Data.ByteString.Lazy.ByteString where
  to =
    IsomorphismClass.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict

instance IsEqualTo Data.Text.Array.Array Data.ByteString.Lazy.ByteString

instance IsEqualTo Data.ByteString.Lazy.ByteString Data.Text.Array.Array

#endif
