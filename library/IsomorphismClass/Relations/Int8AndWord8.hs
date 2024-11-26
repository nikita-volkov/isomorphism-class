{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int8AndWord8 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Int8 Word8 where
  to = fromIntegral

instance IsomorphicToSubsetOf Word8 Int8 where
  to = fromIntegral

instance IsomorphicTo Int8 Word8

instance IsomorphicTo Word8 Int8
