{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict

instance IsSubsetOf Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance IsEqualTo Data.Text.Lazy.Text Text

instance IsEqualTo Text Data.Text.Lazy.Text
