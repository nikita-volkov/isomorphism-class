{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.ShortByteString.TextArray where

#if !MIN_VERSION_text(2,1,0)
  
import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Text.Array as TextArray
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString TextArray.Array where
  to a =
    Data.ByteString.Short.Internal.SBS (IsomorphismClass.TextCompat.Array.toUnliftedByteArray a)
  partiallyFrom (Data.ByteString.Short.Internal.SBS unliftedByteArray) =
    Just (IsomorphismClass.TextCompat.Array.fromUnliftedByteArray unliftedByteArray)

#endif
