module IsomorphismClass.Proxies.ViaIsomorphicToSubsetOf where

import IsomorphismClass.Classes.IsomorphicToSubsetOf
import IsomorphismClass.Prelude
import qualified Test.QuickCheck as QuickCheck

-- | Helper for derivation of instances on types which have an instance of @'IsomorphicToSubsetOf' sup@.
newtype ViaIsomorphicToSubsetOf sup sub = ViaIsomorphicToSubsetOf sub

instance (IsomorphicToSubsetOf sup sub) => IsomorphicToSubsetOf sup (ViaIsomorphicToSubsetOf sup sub) where
  to (ViaIsomorphicToSubsetOf a) = to a
  maybeFrom = fmap ViaIsomorphicToSubsetOf . maybeFrom

instance (IsomorphicToSubsetOf sup sub, Show sup) => Show (ViaIsomorphicToSubsetOf sup sub) where
  show (ViaIsomorphicToSubsetOf a) = show (to @sup a)

instance (IsomorphicToSubsetOf sup sub, Read sup) => Read (ViaIsomorphicToSubsetOf sup sub) where
  readPrec = do
    sup <- readPrec
    case maybeFrom @sup sup of
      Just a -> pure (ViaIsomorphicToSubsetOf a)
      Nothing -> fail "Value is not from the subset"

instance (IsomorphicToSubsetOf sup sub, IsString sup) => IsString (ViaIsomorphicToSubsetOf sup sub) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsomorphicToSubsetOf . maybeFrom @sup . fromString

instance (IsomorphicToSubsetOf sup sub, Eq sup) => Eq (ViaIsomorphicToSubsetOf sup sub) where
  (==) = on (==) (to @sup)

instance (IsomorphicToSubsetOf sup sub, Ord sup) => Ord (ViaIsomorphicToSubsetOf sup sub) where
  compare = on compare (to @sup)

instance (IsomorphicToSubsetOf sup sub, QuickCheck.Arbitrary sup) => QuickCheck.Arbitrary (ViaIsomorphicToSubsetOf sup sub) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @sup)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @sup value)
    shrunkValue
      & maybeFrom
      & maybeToList
