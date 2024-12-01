{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextBuilderAndString where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.StringAndText ()

instance IsSome String Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.unpack . Data.Text.Lazy.Builder.toLazyText
  maybeFrom = fmap Data.Text.Lazy.Builder.fromText . maybeFrom
