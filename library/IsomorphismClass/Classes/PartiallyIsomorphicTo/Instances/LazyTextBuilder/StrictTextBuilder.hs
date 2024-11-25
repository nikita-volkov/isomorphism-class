{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyTextBuilder.StrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import Data.Text.Lazy.Builder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Builder Data.Text.Encoding.StrictBuilder where
  to = fromText . Data.Text.Encoding.strictBuilderToText
  partiallyFrom = Just . Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict . toLazyText

#endif
