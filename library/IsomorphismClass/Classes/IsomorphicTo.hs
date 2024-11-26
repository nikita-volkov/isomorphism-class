module IsomorphismClass.Classes.IsomorphicTo where

import IsomorphismClass.Classes.IsomorphicToSubsetOf

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsomorphicTo a b@ as \"/B/ is isomorphic to /A/\".
--
-- === Laws
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
-- === Instance Definition
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define four instances, namely: @IsomorphicTo A B@ and @IsomorphicTo B A@ as well as @IsomorphicToSubsetOf A B@ and @IsomorphicToSubsetOf B A@.
class (IsomorphicToSubsetOf a b, IsomorphicTo b a) => IsomorphicTo a b

-- | Any type is isomorphic to itself.
instance IsomorphicTo a a

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromText :: IsomorphicTo Text b => Text -> b
-- > fromText = from @Text
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: (IsomorphicTo a b) => a -> b
from = to
