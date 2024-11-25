{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#if MIN_VERSION_text(2,0,2)

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.StrictTextBuilder where

import qualified Data.Text.Encoding as TextEncoding
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.String.Text ()
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo String TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText
  partiallyFrom = fmap TextEncoding.textToStrictBuilder . partiallyFrom

#endif
