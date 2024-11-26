{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int8AndWord8 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Int8 Word8 where
  to = fromIntegral

instance IsSome Word8 Int8 where
  to = fromIntegral

instance Is Int8 Word8

instance Is Word8 Int8
