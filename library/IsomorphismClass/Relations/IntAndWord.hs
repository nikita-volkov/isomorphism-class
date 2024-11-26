{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntAndWord where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Int Word where
  to = fromIntegral

instance IsSubsetOf Word Int where
  to = fromIntegral

instance IsEqualTo Int Word

instance IsEqualTo Word Int
