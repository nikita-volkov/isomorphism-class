-- |
-- Lawful solution to the conversion problem.
--
-- = Conversion problem
--
-- How often do you import @Data.Text.Lazy@ only to call @fromStrict@ or @toStrict@?
-- How about importing @Data.ByteString.Builder@ only to call its
-- 'Data.ByteString.Builder.toLazyByteString' and then importing
-- @Data.ByteString.Lazy@ only to call its 'Data.ByteString.Lazy.toStrict'?
-- How often do you convert from @DiffTime@ to @NominalDiffTime@ and back?
--
-- Those all are instances of one pattern. They are conversions between different
-- representations of the same information. Codebases that don't attempt to
-- abstract over this pattern tend to be sprawling with this type of
-- boilerplate. It's noise to the codereader, it's a burden to the
-- implementor and the maintainer.
--
-- = Why another conversion library?
--
-- Many libraries exist that approach the conversion problem. However most of
-- them provide lawless typeclasses leaving it up to the author of the
-- instance to define what makes a proper conversion. This results in
-- inconsistencies across instances, their behaviour not being evident to
-- the user and no way to check whether an instance is correct.
--
-- This library tackles this problem with a lawful typeclass, making it
-- evident what any of its instances do and it provides a property-test for you
-- to validate your instances.
--
-- = The laws
--
-- The key insight of this library is that if you add a requirement for the
-- conversion to be lossless and to have a mirror conversion in the opposite
-- direction, there usually appears to be only one way of defining it. That
-- makes it very clear what the conversion does to the user and how to define
-- it to the author of the conversion.
-- It also gives a clear criteria for validating whether the instances are correct, which can be encoded in property-tests.
--
-- That insight itself stems from an observation that almost all of the
-- practical conversions in Haskell share a property: you can restore the
-- original data from its converted form. E.g., you can get a text from
-- a text-builder and you can create a text-builder from a text, you can convert
-- a bytestring into a list of bytes and vice-versa, bytestring to\/from bytearray,
-- strict bytestring to\/from lazy, list to\/from sequence, sequence to/from
-- vector, set of ints to\/from int-set. In other words, it's always a two-way
-- street with them and there's a lot of instances of this pattern.
--
-- = UX
--
-- A few other accidental findings like encoding this property with recursive
-- typeclass constraints and fine-tuning for the use of
-- the @TypeApplications@ extension resulted in a terse and clear API.
--
-- Essentially the whole API is just two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The only difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > toText = to @Text
--
-- > fromBuilder = from @Builder
--
-- The types are self-evident:
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
--
-- > > :t from @Builder
-- > from @Builder :: IsomorphicTo Builder b => Builder -> b
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer or the reader.
--
-- = Examples
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> 'Data.ByteString.Lazy.ByteString' -> [Word8]
-- combineEncodings a b c =
--   'from' @'Data.ByteString.Builder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'Data.Text.Encoding.StrictTextBuilder' $
--     "Height of " <> 'to' name <> " is " <> 'fromString' (show height)
-- @
module IsomorphismClass
  ( -- * Typeclasses
    IsomorphicTo (..),
    from,

    -- * Optics
    isomorphicToIso,

    -- * Testing
    module IsomorphismClass.Properties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Optics
import IsomorphismClass.Properties
import IsomorphismClass.Relations ()
