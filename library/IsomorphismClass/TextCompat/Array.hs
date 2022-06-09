{-# LANGUAGE CPP #-}

module IsomorphismClass.TextCompat.Array where

import Data.Text.Array
import GHC.Exts (ByteArray#)
import IsomorphismClass.Prelude

{-# INLINE toUnliftedByteArray #-}
toUnliftedByteArray :: Array -> ByteArray#
#if MIN_VERSION_text(2,0,0)
toUnliftedByteArray (ByteArray a) = a
#else
toUnliftedByteArray (Array a) = a
#endif

{-# INLINE fromUnliftedByteArray #-}
fromUnliftedByteArray :: ByteArray# -> Array
#if MIN_VERSION_text(2,0,0)
fromUnliftedByteArray = ByteArray
#else
fromUnliftedByteArray = Array
#endif
