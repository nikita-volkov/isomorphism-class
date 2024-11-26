{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int32AndWord32 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Int32 Word32 where
  to = fromIntegral

instance IsSubsetOf Word32 Int32 where
  to = fromIntegral

instance IsEqualTo Int32 Word32

instance IsEqualTo Word32 Int32
