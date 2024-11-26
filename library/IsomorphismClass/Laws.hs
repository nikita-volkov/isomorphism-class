module IsomorphismClass.Laws
  ( isomorphicToSubsetOfProperties,
    isomorphicToProperties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import Test.QuickCheck

-- |
-- Properties testing whether an instance satisfies the laws of 'IsomorphicToSubsetOf'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "IsomorphicToSubsetOf laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isomorphicToSubsetOfProperties @Int32 @Int16 Proxy Proxy)
isomorphicToSubsetOfProperties ::
  (IsomorphicToSubsetOf a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isomorphicToSubsetOfProperties superp subp =
  [ ( "'to' is injective",
      property \a b ->
        a /= b ==>
          to' a =/= to' b
    ),
    ( "'maybeFrom' is partially injective",
      property \a b ->
        a /= b ==>
          isJust (maybeFrom' a) ==>
            maybeFrom' a =/= maybeFrom' b
    ),
    ( "'maybeFrom' is an inverse of 'to'",
      property \a ->
        maybeFrom' (to' a) == Just a
    ),
    ( "'to' is an inverse of 'maybeFrom'",
      property \a ->
        fmap to' (maybeFrom' a) == fmap (const a) (maybeFrom' a)
    )
  ]
  where
    to' = as superp . to . as subp
    maybeFrom' = fmap (as subp) . maybeFrom . as superp
    as = flip asProxyTypeOf

-- |
-- Properties testing whether an instance satisfies the laws of 'IsomorphicTo'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "IsomorphicTo laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isomorphicToProperties @Int32 @Word32 Proxy Proxy)
isomorphicToProperties ::
  (IsomorphicTo a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isomorphicToProperties superp subp =
  [ directedLaws "↻" superp subp,
    directedLaws "↺" subp superp
  ]
    & mconcat
  where
    directedLaws prefix ap bp =
      ( ( "Isomorphic: Law 1",
          property \b ->
            b === to (asProxyTypeOf (to (asProxyTypeOf b bp)) ap)
        )
          : prefixEachName "Partially isomorphic: " (isomorphicToSubsetOfProperties ap bp)
      )
        & prefixEachName (prefix <> ": ")

prefixEachName ::
  String ->
  [(String, Property)] ->
  [(String, Property)]
prefixEachName prefix =
  (fmap . first) (mappend prefix)
