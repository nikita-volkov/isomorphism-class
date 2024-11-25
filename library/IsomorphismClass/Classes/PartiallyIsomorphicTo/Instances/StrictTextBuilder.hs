{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Classes.PartiallyIsomorphicTo.Instances.StrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextEncoding.StrictBuilder where
  to = id
  partiallyFrom = Just . id

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder Text where
  to = TextEncoding.textToStrictBuilder
  partiallyFrom = Just . TextEncoding.strictBuilderToText

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextLazy.Text where
  to = TextEncoding.textToStrictBuilder . TextLazy.toStrict
  partiallyFrom = Just . TextLazy.fromStrict . TextEncoding.strictBuilderToText

instance PartiallyIsomorphicTo TextEncoding.StrictBuilder TextLazyBuilder.Builder where
  to = TextEncoding.textToStrictBuilder . TextLazy.toStrict . TextLazyBuilder.toLazyText
  partiallyFrom = Just . TextLazyBuilder.fromText . TextEncoding.strictBuilderToText

#endif
