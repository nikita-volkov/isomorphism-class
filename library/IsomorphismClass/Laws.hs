module IsomorphismClass.Laws
  ( partiallyIsEqualToProperties,
    isomorphicToProperties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import Test.QuickCheck

partiallyIsEqualToProperties ::
  (IsSubsetOf a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
partiallyIsEqualToProperties superp subp =
  [ ( "Law 1",
      property \sub ->
        maybeFrom (asProxyTypeOf (to (asProxyTypeOf sub subp)) superp) === Just sub
    ),
    ( "Law 2",
      property \super ->
        case maybeFrom (asProxyTypeOf super superp) of
          Just sub -> to (asProxyTypeOf sub subp) === super
          Nothing -> property ()
    )
  ]

isomorphicToProperties ::
  (IsEqualTo a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
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
          : prefixEachName "Partially isomorphic: " (partiallyIsEqualToProperties ap bp)
      )
        & prefixEachName (prefix <> ": ")

prefixEachName ::
  String ->
  [(String, Property)] ->
  [(String, Property)]
prefixEachName prefix =
  (fmap . first) (mappend prefix)