module IsomorphismClass.Classes.PartiallyIsomorphicTo.Laws
  ( partiallyIsomorphicToProperties,
  )
where

import IsomorphismClass.Classes.PartiallyIsomorphicTo.Class
import IsomorphismClass.Prelude
import Test.QuickCheck

partiallyIsomorphicToProperties ::
  (PartiallyIsomorphicTo a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
partiallyIsomorphicToProperties superp subp =
  [ ( "Law 1",
      property \sub ->
        partiallyFrom (asProxyTypeOf (to (asProxyTypeOf sub subp)) superp) === Just sub
    ),
    ( "Law 2",
      property \super ->
        case partiallyFrom (asProxyTypeOf super superp) of
          Just sub -> to (asProxyTypeOf sub subp) === super
          Nothing -> property ()
    )
  ]
