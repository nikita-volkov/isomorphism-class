{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndString where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.StringAndText ()

instance IsSome String Data.Text.Lazy.Text where
  to = Data.Text.Lazy.unpack
  maybeFrom = fmap Data.Text.Lazy.fromStrict . maybeFrom
