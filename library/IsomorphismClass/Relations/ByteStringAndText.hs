{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndText where

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSubsetOf ByteString Text where
  to = Data.Text.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'
