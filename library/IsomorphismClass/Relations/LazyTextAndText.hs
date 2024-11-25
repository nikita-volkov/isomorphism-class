{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict
  partiallyFrom = Just . to

instance IsomorphicTo Data.Text.Lazy.Text Text

instance IsomorphicTo Text Data.Text.Lazy.Text
