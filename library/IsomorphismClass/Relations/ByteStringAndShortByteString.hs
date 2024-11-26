{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndShortByteString where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance IsSubsetOf Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort

instance IsEqualTo ByteString Data.ByteString.Short.ShortByteString

instance IsEqualTo Data.ByteString.Short.ShortByteString ByteString
