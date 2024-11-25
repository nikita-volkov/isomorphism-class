{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyTextBuilder where

import qualified Data.Text.Lazy
import Data.Text.Lazy.Builder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyTextBuilder.StrictTextBuilder ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Builder Builder where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo Builder Text where
  to = fromText
  partiallyFrom = Just . Data.Text.Lazy.toStrict . toLazyText

instance PartiallyIsomorphicTo Builder Data.Text.Lazy.Text where
  to = fromLazyText
  partiallyFrom = Just . toLazyText
