{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}

module GDAL.Internal.Types.Vector (
    Vector (..)
  , MVector(..)
) where

import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as GM
import           GDAL.Internal.Types.Vector.Mutable ( MVector(..)
                                                    , newAs
                                                    , mkMVector
                                                    , gWithMutableByteArray
                                                    )

import Foreign.Ptr (Ptr)

import Control.DeepSeq ( NFData(rnf) )
import Control.Monad.Primitive (PrimMonad, touch)

import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.Types (Prim(..))
import Data.Typeable (Typeable)

import Text.Read     ( Read(..), readListPrecDefault )

import GHC.Base
import GHC.Int
import GHC.Ptr (Ptr(..))
import GHC.Word

import GDAL.Internal.Types.Pair (Pair)
import GDAL.Internal.DataType hiding (Vector(..), MVector(..))
import qualified GDAL.Internal.DataType as DT

type Indexer a  = ByteArray# -> Int# -> a

-- | 'GDALType'-based vectors

data Vector a =
  Vector { vLen      :: {-# UNPACK #-} !Int
         , vOff      :: {-# UNPACK #-} !Int
         , vDataType :: {-# UNPACK #-} !DataType
         , vData     :: {-# UNPACK #-} !ByteArray
         , vIndex    ::                !(Indexer a)
         }
  deriving ( Typeable )

instance NFData (Vector a) where
  rnf (Vector _ _ _ _ _) = ()

instance (GDALType a, Show a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (GDALType a, Read a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

type instance G.Mutable Vector = MVector

instance GDALType a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze MVector{mvLen, mvOff, mvDataType, mvData} = do
    arr <- unsafeFreezeByteArray mvData
    return $! Vector { vLen      = mvLen
                     , vOff      = mvOff
                     , vDataType = mvDataType
                     , vData     = arr
                     , vIndex    = gIndex mvDataType
                     }

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw Vector{vLen, vOff, vDataType, vData} = do
    arr <- unsafeThawByteArray vData
    return $! mkMVector vDataType vLen vOff arr

  {-# INLINE basicLength #-}
  basicLength v = vLen v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m v = v { vOff = vOff v + j
                             , vLen = m
                             }

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM Vector{vOff=I# o#,vData=ByteArray arr#,vIndex} (I# i#) =
    return $! vIndex arr# (i# +# o#)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst src = G.unsafeThaw src >>= GM.unsafeCopy dst

  {-# INLINE elemseq #-}
  elemseq _ = seq

gIndex :: forall a. GDALType a => DataType -> Indexer a
gIndex !dt p# i#
  | dt == gdtByte     = indexWith (undefined :: Word8) gFromIntegral
  | dt == gdtUInt16   = indexWith (undefined :: Word16) gFromIntegral
  | dt == gdtUInt32   = indexWith (undefined :: Word32) gFromIntegral
  | dt == gdtInt16    = indexWith (undefined :: Int16) gFromIntegral
  | dt == gdtInt32    = indexWith (undefined :: Int32) gFromIntegral
  | dt == gdtFloat32  = indexWith (undefined :: Float) gFromReal
  | dt == gdtFloat64  = indexWith (undefined :: Double) gFromReal
  | dt == gdtCInt16   = indexWith (undefined :: Pair Int16) gFromIntegralPair
  | dt == gdtCInt32   = indexWith (undefined :: Pair Int32) gFromIntegralPair
  | dt == gdtCFloat32 = indexWith (undefined :: Pair Float) gFromRealPair
  | dt == gdtCFloat64 = indexWith (undefined :: Pair Double) gFromRealPair
  | otherwise         = error "gIndex: Invalid GType"
  where
    {-# INLINE indexWith #-}
    indexWith :: Prim b => b -> (b -> a) -> a
    indexWith y f = inline f (inline indexByteArray# p# i# `asTypeOf` y)
{-# INLINE gIndex #-}



unsafeWithDataType
  :: Vector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithDataType Vector{vDataType, vOff, vData} f =
  gUnsafeWithByteArray vDataType vOff vData (f vDataType)
{-# INLINE unsafeWithDataType #-}

unsafeAsDataType
  :: GDALType a => DataType -> Vector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt v@Vector{vDataType=dt', vOff=off, vData=arr, vLen=n} f
  | dt == dt' = gUnsafeWithByteArray dt off arr f
  | otherwise = do
      copy <- newAs dt n
      G.unsafeCopy copy v
      gWithMutableByteArray dt (mvOff copy) (mvData copy) f
{-# INLINE unsafeAsDataType #-}

gUnsafeWithByteArray
  :: PrimMonad m
  => DataType
  -> Int
  -> ByteArray
  -> (Ptr () -> m a)
  -> m a
gUnsafeWithByteArray dt off a f = do
  r <- f (Ptr addr)
  touch a
  return r
  where !(Addr addr) = byteArrayContents a `plusAddr` byteOff
        !byteOff     = off * sizeOfDataType dt
{-# INLINE gUnsafeWithByteArray #-}

instance GDALType a => DT.Vector Vector a where
  gUnsafeAsDataType = unsafeAsDataType
  gUnsafeWithDataType = unsafeWithDataType
  {-# INLINE gUnsafeWithDataType #-}
  {-# INLINE gUnsafeAsDataType #-}
