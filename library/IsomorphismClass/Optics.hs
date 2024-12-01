module IsomorphismClass.Optics where

import Data.Profunctor
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

-- | Van-Laarhoven-style Prism, compatible with the \"lens\" library.
isSomePrism :: (IsSome a b, Choice p, Applicative f) => p b (f b) -> p a (f a)
isSomePrism =
  dimap
    (\s -> maybe (Left s) Right (maybeFrom s))
    (either pure (fmap to))
    . right'

-- | Van-Laarhoven-style Isomorphism, compatible with the \"lens\" library.
isIso :: (Is a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isIso = dimap from (fmap to)
