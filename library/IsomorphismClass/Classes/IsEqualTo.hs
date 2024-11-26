module IsomorphismClass.Classes.IsEqualTo where

import IsomorphismClass.Classes.IsSubsetOf

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @IsEqualTo a b@ as \"/B/ is equal to /A/\".
--
-- __Laws__
--
-- /B/ is equal to /A/ if and only if there exists a conversion from /B/
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
-- __Instance Definition__
--
-- For each pair of equal types (/A/ and /B/) the compiler will require
-- you to define four instances, namely: @IsEqualTo A B@ and @IsEqualTo B A@ as well as @IsSubsetOf A B@ and @IsSubsetOf B A@.
class (IsSubsetOf a b, IsEqualTo b a) => IsEqualTo a b

instance IsEqualTo a a

-- |
-- 'to' in reverse direction.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the input type, e.g.:
--
-- > fromText :: IsEqualTo Text b => Text -> b
-- > fromText = from @Text
--
-- The first type application of the 'to' function on the other hand specifies
-- the output data type.
from :: (IsEqualTo a b) => a -> b
from = to
