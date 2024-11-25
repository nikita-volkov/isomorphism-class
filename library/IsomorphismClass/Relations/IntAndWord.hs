{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntAndWord where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Int Word where
  to = fromIntegral

instance PartiallyIsomorphicTo Word Int where
  to = fromIntegral

instance IsomorphicTo Int Word

instance IsomorphicTo Word Int
