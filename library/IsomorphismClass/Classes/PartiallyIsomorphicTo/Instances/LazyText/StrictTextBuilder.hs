{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.LazyText.StrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText
  partiallyFrom = Just . Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

#endif
