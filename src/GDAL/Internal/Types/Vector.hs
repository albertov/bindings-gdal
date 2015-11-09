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
  , unsafeWithDataType
  , unsafeAsDataType
) where

import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as GM
import           GDAL.Internal.Types.Vector.Mutable ( MVector(..), newAs
                                                    , mkMVector
                                                    )

import Foreign.Ptr (Ptr)

import Control.DeepSeq ( NFData(rnf) )

import Data.Primitive.ByteArray
--import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import Text.Read     ( Read(..), readListPrecDefault )

import GHC.Base (Int(..))
import GHC.Exts (inline)
import GDAL.Internal.DataType

-- | 'GDALType'-based vectors

data Vector a =
  Vector { vLen      :: {-# UNPACK #-} !Int
         , vOff      :: {-# UNPACK #-} !Int
         , vDataType :: {-# UNPACK #-} !DataType
         , vData     :: {-# UNPACK #-} !ByteArray
         }
  deriving ( Typeable )

instance NFData (Vector a) where
  rnf (Vector _ _ _ _) = ()

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
  basicUnsafeIndexM Vector{vOff,vData=ByteArray arr#,vDataType} i =
    return $! inline gIndex vDataType arr# ix#
    where !(I# ix#) = vOff + i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst src = G.unsafeThaw src >>= GM.unsafeCopy dst

  {-# INLINE elemseq #-}
  elemseq _ = seq

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
