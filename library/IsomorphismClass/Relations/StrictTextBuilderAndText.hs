{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicToSubsetOf Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsomorphicToSubsetOf Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

instance IsomorphicTo Text Data.Text.Encoding.StrictBuilder

instance IsomorphicTo Data.Text.Encoding.StrictBuilder Text

#endif
