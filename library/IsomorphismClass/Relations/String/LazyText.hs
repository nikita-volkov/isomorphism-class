{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.String.LazyText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.String.Text ()

instance PartiallyIsomorphicTo String Data.Text.Lazy.Text where
  to = Data.Text.Lazy.unpack
  partiallyFrom = fmap Data.Text.Lazy.fromStrict . partiallyFrom
