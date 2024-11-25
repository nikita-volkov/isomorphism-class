{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.Text where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.Text.StrictTextBuilder ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Text Text where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict
  partiallyFrom = Just . Data.Text.Lazy.fromStrict

instance PartiallyIsomorphicTo Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
  partiallyFrom = Just . Data.Text.Lazy.Builder.fromText
