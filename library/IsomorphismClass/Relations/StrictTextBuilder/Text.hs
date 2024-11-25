{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.StrictTextBuilder.Text where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText
  partiallyFrom = Just . to

instance PartiallyIsomorphicTo Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder
  partiallyFrom = Just . to

instance IsomorphicTo Text Data.Text.Encoding.StrictBuilder

instance IsomorphicTo Data.Text.Encoding.StrictBuilder Text

#endif
