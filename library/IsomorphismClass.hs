{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module IsomorphismClass where

import qualified Data.Vector.Unboxed as VectorUnboxed

class IsomorphicTo b a => IsomorphicTo a b where
  to :: b -> a

instance VectorUnboxed.Unbox a => IsomorphicTo [a] (VectorUnboxed.Vector a) where
  to = VectorUnboxed.toList

instance VectorUnboxed.Unbox a => IsomorphicTo (VectorUnboxed.Vector a) [a] where
  to = VectorUnboxed.fromList
