module IsomorphismClass.Classes.IsomorphicTo where

import IsomorphismClass.Classes.PartiallyIsomorphicTo
import IsomorphismClass.Prelude

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsomorphicTo a b@ as \"/B/ is isomorphic to /A/\".
--
-- __Laws__
--
-- /B/ is isomorphic to /A/ if and only if there exists a conversion from /B/
-- to /A/ ('to') and a conversion from /A/ to /B/ ('from') such that:
--
-- - @'from' . 'to' = 'id'@ - For all values of /B/ converting from /B/ to /A/
--     and then converting from /A/ to /B/ produces a value that is identical
--     to the original.
--
-- - @'to' . 'from' = 'id'@ - For all values of /A/ converting from /A/ to /B/
--     and then converting from /B/ to /A/ produces a value that is identical
--     to the original.
--
-- __Usage__
--
-- This class is particularly easy to use in combination with
-- the @TypeApplications@ extension making it clear to the reader what sort
-- of conversion he sees. E.g.,
--
-- > fromString = from @String
--
-- > toText = to @Text
--
-- The types are also self-evident:
--
-- > > :t from @String
-- > from @String :: IsomorphicTo b String => String -> b
--
-- > > :t to @Text
-- > to @Text :: IsomorphicTo Text b => b -> Text
--
-- __Instance Definition__
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define two instances, namely: @IsomorphicTo A B@ and @IsomorphicTo
-- B A@.
class (PartiallyIsomorphicTo a b, IsomorphicTo b a) => IsomorphicTo a b

instance IsomorphicTo a a

instance IsomorphicTo [a] (Seq a)

instance IsomorphicTo (Seq a) [a]

instance IsomorphicTo (Set Int) IntSet

instance IsomorphicTo IntSet (Set Int)

instance IsomorphicTo (Map Int v) (IntMap v)

instance IsomorphicTo (IntMap v) (Map Int v)

instance IsomorphicTo Int Word

instance IsomorphicTo Int16 Word16

instance IsomorphicTo Int32 Word32

instance IsomorphicTo Int64 Word64

instance IsomorphicTo Int8 Word8

instance IsomorphicTo Word Int

instance IsomorphicTo Word16 Int16

instance IsomorphicTo Word32 Int32

instance IsomorphicTo Word64 Int64

instance IsomorphicTo Word8 Int8

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromText :: IsomorphicTo Text a => Text -> a
-- > fromText = from @Text
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: (IsomorphicTo a b) => a -> b
from = to
