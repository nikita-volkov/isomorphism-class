{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.String.StrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Relations.String.Text ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo String Data.Text.Encoding.StrictBuilder where
  to = to . Data.Text.Encoding.strictBuilderToText
  partiallyFrom = fmap Data.Text.Encoding.textToStrictBuilder . partiallyFrom

#endif
