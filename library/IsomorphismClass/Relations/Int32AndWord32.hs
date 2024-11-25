{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int32AndWord32 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Int32 Word32 where
  to = fromIntegral

instance PartiallyIsomorphicTo Word32 Int32 where
  to = fromIntegral

instance IsomorphicTo Int32 Word32

instance IsomorphicTo Word32 Int32
