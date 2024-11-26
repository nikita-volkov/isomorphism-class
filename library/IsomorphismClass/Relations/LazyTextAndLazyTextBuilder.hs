{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndLazyTextBuilder where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Data.Text.Lazy.Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.Builder.toLazyText
  maybeFrom = Just . Data.Text.Lazy.Builder.fromLazyText

instance IsomorphicToSubsetOf Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text where
  to = Data.Text.Lazy.Builder.fromLazyText
  maybeFrom = Just . Data.Text.Lazy.Builder.toLazyText

instance IsomorphicTo Data.Text.Lazy.Text Data.Text.Lazy.Builder.Builder

instance IsomorphicTo Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text
