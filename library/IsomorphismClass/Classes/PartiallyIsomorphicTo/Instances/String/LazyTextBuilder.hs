{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.LazyTextBuilder where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.Text ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo String Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.unpack . Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = fmap Data.Text.Lazy.Builder.fromText . partiallyFrom
