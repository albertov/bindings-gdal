{-# LANGUAGE CPP, UnboxedTuples, MagicHash, ScopedTypeVariables #-}

module GDAL.Internal.Types.Pair (Pair(..)) where

import Data.Primitive.Types
import GHC.Base

newtype Pair a = Pair { unPair :: (a, a) }

instance Functor Pair where
  fmap f (Pair (a, b)) = Pair (f a, f b)
  {-# INLINE fmap #-}

instance Prim a => Prim (Pair a) where

  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

  sizeOf# _    = sizeOf# (undefined :: a) *# 2#
  alignment# _ = alignment# (undefined :: a)

  indexByteArray# arr# i# = Pair ( indexByteArray# arr# (i# *# 2#)
                                 , indexByteArray# arr# (i# *# 2# +# 1#)
                                 )
  readByteArray# a# i# s# =
    case readByteArray# a# (i# *# 2#) s# of
      (# s1#, x #) ->
        case readByteArray# a# (i# *# 2# +# 1#) s1# of
          (# s2#, y #) -> (# s2# , Pair (x, y) #)

  writeByteArray# a# i# (Pair (x,y)) s# =
    case writeByteArray# a# (i# *# 2#) x s# of
      s1# -> writeByteArray# a# (i# *# 2# +# 1#) y s1#

  setByteArray# arr# off# n# v s# = go 0# s#
    where
      go i# s'# = case i# <# n# of
                    0# -> s'#
                    _  -> case writeByteArray# arr# (i# +# off#) v s'# of
                                 s''# -> go (i# +# 1#) s''#

  indexOffAddr# a# i# = Pair ( indexOffAddr# a# (i# *# 2#)
                             , indexOffAddr# a# (i# *# 2# +# 1#)
                             )
  readOffAddr# a# i# s# =
    case readOffAddr# a# (i# *# 2#) s# of
      (# s1#, x #) ->
        case readOffAddr# a# (i# *# 2# +# 1#) s1# of
          (# s2#, y #) -> (# s2# , Pair (x, y) #)

  writeOffAddr# a# i# (Pair (x,y)) s# =
    case writeOffAddr# a# (i# *# 2#) x s# of
      s1# -> writeOffAddr# a# (i# *# 2# +# 1#) y s1#

  setOffAddr# a# off# n# v s# = go 0# s#
    where
      go i# s'# = case i# <# n# of
                    0# -> s'#
                    _  -> case writeOffAddr# a# (i# +# off#) v s'# of
                                 s''# -> go (i# +# 1#) s''#
