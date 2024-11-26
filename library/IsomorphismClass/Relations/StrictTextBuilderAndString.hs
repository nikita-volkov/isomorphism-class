{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.StrictTextBuilderAndString where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Relations.StringAndText ()
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf String Data.Text.Encoding.StrictBuilder where
  to = to . Data.Text.Encoding.strictBuilderToText
  maybeFrom = fmap Data.Text.Encoding.textToStrictBuilder . maybeFrom

#endif
