{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int32AndWord32 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Int32 Word32 where
  to = fromIntegral

instance IsSome Word32 Int32 where
  to = fromIntegral

instance Is Int32 Word32

instance Is Word32 Int32
