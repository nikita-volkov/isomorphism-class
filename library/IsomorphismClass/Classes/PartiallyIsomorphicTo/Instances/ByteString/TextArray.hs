{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.ByteString.TextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance PartiallyIsomorphicTo ByteString Data.Text.Array.Array where
  to = Data.ByteString.Short.fromShort . IsomorphismClass.TextCompat.Array.toShortByteString
  partiallyFrom = Just . IsomorphismClass.TextCompat.Array.fromShortByteString . Data.ByteString.Short.toShort

#endif
