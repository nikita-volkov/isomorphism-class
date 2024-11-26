{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict

instance IsomorphicToSubsetOf Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance IsomorphicTo Data.Text.Lazy.Text Text

instance IsomorphicTo Text Data.Text.Lazy.Text
