{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.Text.StrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText
  partiallyFrom = Just . Data.Text.Encoding.textToStrictBuilder

#endif
