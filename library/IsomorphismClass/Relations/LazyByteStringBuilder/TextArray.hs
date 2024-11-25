{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringBuilder.TextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.Text.Array.Array where
  to = Data.ByteString.Builder.shortByteString . IsomorphismClass.TextCompat.Array.toShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Text.Array.Array Data.ByteString.Builder.Builder where
  to =
    IsomorphismClass.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict
      . Data.ByteString.Builder.toLazyByteString
  partiallyFrom = Just . to

instance IsomorphicTo Data.Text.Array.Array Data.ByteString.Builder.Builder

instance IsomorphicTo Data.ByteString.Builder.Builder Data.Text.Array.Array

#endif
