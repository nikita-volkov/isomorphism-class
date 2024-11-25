{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteString.TextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance PartiallyIsomorphicTo ByteString Data.Text.Array.Array where
  to = Data.ByteString.Short.fromShort . IsomorphismClass.TextCompat.Array.toShortByteString
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Text.Array.Array ByteString where
  to = IsomorphismClass.TextCompat.Array.fromShortByteString . Data.ByteString.Short.toShort
  partiallyFrom = Just . to

instance IsomorphicTo ByteString Data.Text.Array.Array

instance IsomorphicTo Data.Text.Array.Array ByteString

#endif
