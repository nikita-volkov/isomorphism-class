{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int64AndWord64 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Int64 Word64 where
  to = fromIntegral

instance IsSubsetOf Word64 Int64 where
  to = fromIntegral

instance IsEqualTo Int64 Word64

instance IsEqualTo Word64 Int64
