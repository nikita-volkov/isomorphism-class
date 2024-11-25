{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.String.LazyTextBuilder where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.String.Text ()

instance PartiallyIsomorphicTo String Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.unpack . Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = fmap Data.Text.Lazy.Builder.fromText . partiallyFrom
