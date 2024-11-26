{-# LANGUAGE CPP #-}

-- |
-- Isomorphism as a lawful solution to the conversion problem.
--
-- = Conversion problem
--
-- Have you ever looked for a @toString@ function? How often do you
-- import @Data.Text.Lazy@ only to call its 'Data.Text.Lazy.fromStrict'? How
-- about importing @Data.Text@ only to call its 'Data.Text.unpack'? How
-- about going thru the always fun sequence of
-- importing @Data.ByteString.Builder@ only to call its
-- 'Data.ByteString.Builder.toLazyByteString' and then importing
-- @Data.ByteString.Lazy@ only to call its 'Data.ByteString.Lazy.toStrict'?
--
-- Those all are instances of one pattern. They are conversions between
-- representations of the same information. Codebases that don't attempt to
-- abstract over this pattern tend to be sprawling with this type of
-- boilerplate. It's noise to the codereader, it's a burden to the
-- implementor and the maintainer.
--
-- = Why another conversion library?
--
-- Many libraries exist that approach the conversion problem. However all of
-- them provide lawless typeclasses leaving it up to the author of the
-- instance to define what makes a proper conversion. This results in
-- inconsistencies across instances and their behaviour being not evident to
-- the user.
--
-- This library tackles this problem with a lawful typeclass, making it
-- evident what any of its instances do.
--
-- = The law
--
-- The key insight of this library is that if you add a requirement for the
-- conversion to be lossless and to have a mirror conversion in the opposite
-- direction, there usually appears to be only one way of defining it. That
-- makes it very clear what the conversion does to the user and how to define
-- it to the author of the conversion.
--
-- That insight itself stems from an observation that almost all of the
-- practical conversions in Haskell share a property: you can restore the
-- original data from its converted form. E.g., you can get a bytestring from
-- a builder and you can create a builder from a bytestring, you can convert
-- a text into a list of chars and vice-versa, bytestring to\/from bytearray,
-- strict bytestring to\/from lazy, list to\/from sequence, sequence to/from
-- vector, set of ints to\/from int-set. In other words, it's always a two-way
-- street with them and there's a lot of instances of this pattern.
--
-- = UX
--
-- A few other accidental findings like encoding this property with recursive
-- typeclass constraints and fine-tuning for the use of
-- the @TypeApplications@ extension resulted in a very terse yet clear API.
--
-- Essentially the whole API is just two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The only difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > fromString = from @String
--
-- > toText = to @Text
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer.
--
-- Here are more practical examples:
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'TextLazyBuilder.Builder' $
--     "Height of " <> 'to' name <> " is " <> 'showAs' height
-- @
--
-- @
-- combineEncodings :: 'ByteStringShort.ShortByteString' -> 'PrimitiveByteArray.ByteArray' -> 'ByteString' -> [Word8]
-- combineEncodings a b c =
--   'from' @'ByteStringBuilder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
module IsomorphismClass
  ( -- * Typeclasses
    IsSubsetOf (..),
    IsEqualTo,
    from,

    -- * Testing
    module IsomorphismClass.Laws,

    -- * FAQ

    -- |
    -- = Why no instance for Text/ByteString?
    --
    -- It is not a total isomorphism. Yes, you can represent every Text value using ByteString.
    -- However, not every ByteString can be decoded as valid Text.
    -- It doesn't matter which encoding you apply: UTF8, ISO-8859 or any other.
    --
    -- = String/Text is not exactly a valid isomorphism
    --
    -- Yes. It does not make a valid isomorphism. It is an exception,
    -- due to the ubiquity of String-oriented APIs.
    --
    -- = Are Int64/Word64 really isomorphic?
    --
    -- Yes. Negative integer values get mapped to the upper value range of Word64.
    -- Mapping between those types happens in bits using the 'fromIntegral' function.
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Laws
import IsomorphismClass.Relations ()
