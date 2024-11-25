{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextBuilder.Text where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.Text.Lazy.Builder.Builder Text where
  to = Data.Text.Lazy.Builder.fromText
  partiallyFrom = Just . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText

instance PartiallyIsomorphicTo Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = Just . Data.Text.Lazy.Builder.fromText

instance IsomorphicTo Data.Text.Lazy.Builder.Builder Text

instance IsomorphicTo Text Data.Text.Lazy.Builder.Builder