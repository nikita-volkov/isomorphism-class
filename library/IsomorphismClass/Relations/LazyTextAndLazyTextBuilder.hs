{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndLazyTextBuilder where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = Just . Data.Text.Lazy.Builder.fromLazyText

instance PartiallyIsomorphicTo Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text where
  to = Data.Text.Lazy.Builder.fromLazyText
  partiallyFrom = Just . Data.Text.Lazy.Builder.toLazyText

instance IsomorphicTo Data.Text.Lazy.Text Data.Text.Lazy.Builder.Builder

instance IsomorphicTo Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text
