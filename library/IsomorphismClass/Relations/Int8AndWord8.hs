{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int8AndWord8 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Int8 Word8 where
  to = fromIntegral

instance IsSubsetOf Word8 Int8 where
  to = fromIntegral

instance IsEqualTo Int8 Word8

instance IsEqualTo Word8 Int8
