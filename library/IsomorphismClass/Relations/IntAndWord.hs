{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntAndWord where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Int Word where
  to = fromIntegral

instance IsomorphicToSubsetOf Word Int where
  to = fromIntegral

instance IsomorphicTo Int Word

instance IsomorphicTo Word Int
