{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GDAL.Internal.Types.Vector.Mutable(
    MVector(..)
  , IOVector
  , STVector

  , newAs
  , mkMVector
  , unsafeWithDataType
  , unsafeAsDataType
) where

import Control.DeepSeq ( NFData(rnf) )

import qualified Data.Vector.Generic.Mutable  as G

import Foreign.Ptr (Ptr)

import Control.Monad.Primitive
import Data.Primitive.Types (Prim(..))

import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Base (Int(..), (+#))

import Data.Primitive.ByteArray
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import GDAL.Internal.DataType

-- | Mutable 'GDALType'-based vectors
data MVector s a =
  MVector { mvLen      :: {-# UNPACK #-} !Int
          , mvOff      :: {-# UNPACK #-} !Int
          , mvDataType :: {-# UNPACK #-} !DataType
          , mvData     :: {-# UNPACK #-} !(MutableByteArray s)
          , mvRead     :: !(Reader s a)
          , mvWrite    :: !(Writer s a)
          }
  deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _ _ _ _ _) = ()

instance GDALType a => G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength v = mvLen v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m v = v { mvOff = mvOff v + j
                             , mvLen = m
                             }

  {-# INLINE basicOverlaps #-}
  basicOverlaps MVector{mvOff=i, mvLen=m, mvData=arr1, mvDataType=dt1}
                MVector{mvOff=j, mvLen=n, mvData=arr2, mvDataType=dt2}
    = dt1 == dt2
      && sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  basicUnsafeNew i = newAs (dataType (Proxy :: Proxy a)) i
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize MVector{mvOff=off, mvLen=n, mvData=v, mvDataType=dt} =
      setByteArray v (off * size) (n * size) (0 :: Word8)
    where
      size = sizeOfDataType dt
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead MVector{ mvData=MutableByteArray arr#
                         , mvOff =I# o#
                         , mvRead
                         } (I# i#) = primitive (mvRead arr# (i# +# o#))

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite MVector{ mvData=MutableByteArray arr#
                          , mvOff=I# o#
                          , mvWrite
                          } (I# i#) x = primitive_ (mvWrite arr# (i# +# o#) x)

  {-# INLINE basicSet #-}
  basicSet v x = gdalVectorSet v x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy MVector{mvOff=dOff, mvLen=dLen, mvData=dArr, mvDataType=dDt}
                  MVector{mvOff=sOff, mvData=sArr, mvDataType=sDt} =
    gCopyMutableByteArray dArr dDt dOff sArr sDt sOff dLen

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove MVector{mvOff=i, mvLen=n, mvData=dst, mvDataType=dDt}
                  MVector{mvOff=j, mvData=src, mvDataType=sDt}
    | dDt == sDt = moveByteArray dst (i*sz) src (j*sz) (n * sz)
    | otherwise  = gCopyMutableByteArray dst dDt i src sDt j n
    where sz = sizeOfDataType dDt



newAs
  :: forall m a. (GDALType a, PrimMonad m)
  => DataType -> Int -> m (MVector (PrimState m) a)
newAs dt n
  | n < 0 = error $ "GDAL.Vector.new: negative length: " ++ show n
  | n > mx = error $ "GDAL.Vector.new: length too large: " ++ show n
  | otherwise = do
      arr <- newPinnedByteArray (n*size)
      return $! mkMVector dt n 0 arr
  where
    size = sizeOfDataType dt
    mx = maxBound `quot` size :: Int
{-# INLINE newAs #-}

mkMVector
  :: forall s a. GDALType a
  => DataType -> Int -> Int -> MutableByteArray s -> MVector s a
mkMVector dt len off arr =
  MVector { mvLen      = len
          , mvOff      = off
          , mvDataType = dt
          , mvData     = arr
          , mvRead     = gRead dt
          , mvWrite    = gWrite dt
          }
{-# INLINE mkMVector #-}

gdalVectorSet
  :: forall m a. (GDALType a, PrimMonad m)
  => MVector (PrimState m) a -> a -> m ()
gdalVectorSet v@MVector{mvLen=n, mvDataType=dp, mvData} x
  | n == 0 = return ()
  | otherwise = case sizeOfDataType dp of
                  1 -> gdalVectorSetAsPrim (undefined :: Word8)
                  2 -> gdalVectorSetAsPrim (undefined :: Word16)
                  4 -> gdalVectorSetAsPrim (undefined :: Word32)
                  8 -> gdalVectorSetAsPrim (undefined :: Word64)
                  _ -> let do_set !i
                             | i<n = G.basicUnsafeWrite v i x  >> do_set (i+1)
                             | otherwise = return ()
                       in do_set 0

  where
    {-# INLINE[0] gdalVectorSetAsPrim #-}
    gdalVectorSetAsPrim :: Prim b => b -> m ()
    gdalVectorSetAsPrim y = do
      G.basicUnsafeWrite v 0 x
      w <- readByteArray mvData 0
      setByteArray mvData 1 (n-1) (w `asTypeOf` y)
{-# INLINE gdalVectorSet #-}


unsafeWithDataType :: IOVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithDataType MVector{mvDataType, mvOff, mvData} f =
  gWithMutableByteArray mvDataType mvOff mvData (f mvDataType)
{-# INLINE unsafeWithDataType #-}

unsafeAsDataType
  :: GDALType a
  => DataType -> IOVector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt v@MVector{mvDataType=dt', mvOff=off, mvData=arr, mvLen=n} f
  | dt == dt' = gWithMutableByteArray dt off arr f
  | otherwise = do
      copy <- newAs dt n
      G.unsafeCopy copy v
      gWithMutableByteArray dt (mvOff copy) (mvData copy) f
{-# INLINE unsafeAsDataType #-}
