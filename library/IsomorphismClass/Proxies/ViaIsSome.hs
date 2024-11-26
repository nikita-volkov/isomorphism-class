module IsomorphismClass.Proxies.ViaIsSome where

import IsomorphismClass.Classes.IsSome
import IsomorphismClass.Prelude
import qualified Test.QuickCheck as QuickCheck

-- | Helper for derivation of instances on types which have an instance of @'IsSome' sup@.
newtype ViaIsSome sup sub = ViaIsSome sub

instance (IsSome sup sub) => IsSome sup (ViaIsSome sup sub) where
  to (ViaIsSome a) = to a
  maybeFrom = fmap ViaIsSome . maybeFrom

instance (IsSome sup sub, Show sup) => Show (ViaIsSome sup sub) where
  show (ViaIsSome a) = show (to @sup a)

instance (IsSome sup sub, Read sup) => Read (ViaIsSome sup sub) where
  readPrec = do
    sup <- readPrec
    case maybeFrom @sup sup of
      Just a -> pure (ViaIsSome a)
      Nothing -> fail "Value is not from the subset"

instance (IsSome sup sub, IsString sup) => IsString (ViaIsSome sup sub) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsSome . maybeFrom @sup . fromString

instance (IsSome sup sub, Eq sup) => Eq (ViaIsSome sup sub) where
  (==) = on (==) (to @sup)

instance (IsSome sup sub, Ord sup) => Ord (ViaIsSome sup sub) where
  compare = on compare (to @sup)

instance (IsSome sup sub, QuickCheck.Arbitrary sup) => QuickCheck.Arbitrary (ViaIsSome sup sub) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @sup)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @sup value)
    shrunkValue
      & maybeFrom
      & maybeToList
