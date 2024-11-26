module IsomorphismClass.Classes.IsSubsetOf where

import IsomorphismClass.Prelude

-- |
-- Evidence that @sub@ is a subset of @super@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- - @'maybeFrom' . 'to' = 'Just'@ - For all values of @sub@ converting @sub@ to @super@ and then and attempting to convert back to @sub@ always succeeds and produces a value that is identical to the original.
--
-- - @\a -> fmap 'to' ('maybeFrom' a) = fmap (const a) ('maybeFrom' a)@ - For all values of @super@ attempting to convert to @sub@ and then convert back on success produces the same result as the original if the conversion succeeds.
class IsSubsetOf super sub where
  to :: sub -> super
  maybeFrom :: super -> Maybe sub
  default maybeFrom :: (IsSubsetOf sub super) => super -> Maybe sub
  maybeFrom = Just . to

instance IsSubsetOf a a where
  to = id
  maybeFrom = Just . id

instance IsSubsetOf () sub where
  to = const ()
  maybeFrom = const Nothing

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSubsetOf super Void where
  to = absurd
  maybeFrom = const Nothing
