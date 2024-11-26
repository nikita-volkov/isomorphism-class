{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int16AndWord16 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Int16 Word16 where
  to = fromIntegral

instance IsSubsetOf Word16 Int16 where
  to = fromIntegral

instance IsEqualTo Int16 Word16

instance IsEqualTo Word16 Int16
