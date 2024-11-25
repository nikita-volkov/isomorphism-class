{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.String.Text where

import qualified Data.Text as Text
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo String Text where
  to = Text.unpack
  partiallyFrom string =
    -- FIXME: Optimize.
    let text = Text.pack string
     in if string == Text.unpack text
          then Just text
          else Nothing
