module IsomorphismClass.Laws
  ( isomorphicToSubsetOfProperties,
    isomorphicToProperties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import Test.QuickCheck

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
