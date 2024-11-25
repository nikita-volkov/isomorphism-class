{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.LazyText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.Text ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo String Data.Text.Lazy.Text where
  to = Data.Text.Lazy.unpack
  partiallyFrom = fmap Data.Text.Lazy.fromStrict . partiallyFrom
