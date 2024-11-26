{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.StringAndText where

import qualified Data.Text as Text
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf String Text where
  to = Text.unpack
  maybeFrom string =
    -- FIXME: Optimize.
    let text = Text.pack string
     in if string == Text.unpack text
          then Just text
          else Nothing
