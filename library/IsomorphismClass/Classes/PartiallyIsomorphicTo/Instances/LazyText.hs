{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyText where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyText.StrictTextBuilder ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Data.Text.Lazy.Text where
  to = id
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict
  partiallyFrom = Just . Data.Text.Lazy.toStrict

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = Just . Data.Text.Lazy.Builder.fromLazyText
