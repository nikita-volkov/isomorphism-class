module Main where

import qualified Data.Vector.Unboxed as VectorUnboxed
import Data.Word (Word8)
import IsomorphismClass

main =
  print
    (to ([1, 2, 3] :: [Word8]) :: VectorUnboxed.Vector Word8)
