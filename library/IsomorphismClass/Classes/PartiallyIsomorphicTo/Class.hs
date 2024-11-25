module IsomorphismClass.Classes.PartiallyIsomorphicTo.Class where

import IsomorphismClass.Prelude

-- |
-- Evidence that @sub@ is a proper subset of @super@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- - @'partiallyFrom' . 'to' = 'Just'@ - For all values of @sub@ converting @sub@ to @super@ and then and attempting to convert back to @sub@ always succeeds and produces a value that is identical to the original.
--
-- - @\a -> fmap 'to' ('partiallyFrom' a) = fmap (const a) ('partiallyFrom' a)@ - For all values of @super@ attempting to convert to @sub@ and then convert back on success produces the same result as the original if the conversion succeeds.
class PartiallyIsomorphicTo super sub where
  to :: sub -> super
  partiallyFrom :: super -> Maybe sub
