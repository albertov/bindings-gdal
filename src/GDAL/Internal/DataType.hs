{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Internal.DataType (
    DataType (..)
  , GDALType (..)
  , DataTypeK (..)
  , IsComplex
  , GDT_Byte
  , GDT_UInt16
  , GDT_UInt32
  , GDT_Int16
  , GDT_Int32
  , GDT_Float32
  , GDT_Float64
  , GDT_CInt16
  , GDT_CInt32
  , GDT_CFloat32
  , GDT_CFloat64

  , sizeOfDataType
  , hsDataType
  , dataTypeK
) where

import GDAL.Internal.Types (Pair(..), pFst)
import GDAL.Internal.DataType.Internal
import Data.Word
import Data.Int
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Coerce (coerce)

import Foreign.Storable (Storable(sizeOf))
import Foreign.C.Types (CDouble(..))
import GHC.TypeLits


data DataType :: * -> * where
  GDT_Byte     :: DataType Word8
  GDT_UInt16   :: DataType Word16
  GDT_UInt32   :: DataType Word32
  GDT_Int16    :: DataType Int16
  GDT_Int32    :: DataType Int32
  GDT_Float32  :: DataType Float
  GDT_Float64  :: DataType Double
  GDT_CInt16   :: DataType (Pair Int16)
  GDT_CInt32   :: DataType (Pair Int32)
  GDT_CFloat32 :: DataType (Pair Float)
  GDT_CFloat64 :: DataType (Pair Double)

instance Show (DataType a) where
  show = show . dataTypeK

instance Enum (DataType a) where
  fromEnum = fromEnum . dataTypeK
  toEnum   = error "toEnum (DataType a) is not implemented"

dataTypeK :: DataType a -> DataTypeK
dataTypeK GDT_Byte      = GByte
dataTypeK GDT_UInt16    = GUInt16
dataTypeK GDT_UInt32    = GUInt32
dataTypeK GDT_Int16     = GInt16
dataTypeK GDT_Int32     = GInt32
dataTypeK GDT_Float32   = GFloat32
dataTypeK GDT_Float64   = GFloat64
dataTypeK GDT_CInt16    = GCInt16
dataTypeK GDT_CInt32    = GCInt32
dataTypeK GDT_CFloat32  = GCFloat32
dataTypeK GDT_CFloat64  = GCFloat64


class ( Storable a
      , Eq a
      , Show a
      , Num a
      , Typeable a
      , KnownNat (SizeOf a)
      ) => GDALType a where
  type SizeOf a :: Nat
  dataType    :: DataType a
  toCDouble   :: a -> CDouble
  fromCDouble :: CDouble -> a


type family IsComplex a where
  IsComplex (Pair a)  = 'True
  IsComplex a         = 'False

sizeOfDataType :: DataTypeK -> Int
sizeOfDataType GByte     = sizeOf (undefined :: Word8)
sizeOfDataType GUInt16   = sizeOf (undefined :: Word16)
sizeOfDataType GUInt32   = sizeOf (undefined :: Word32)
sizeOfDataType GInt16    = sizeOf (undefined :: Int16)
sizeOfDataType GInt32    = sizeOf (undefined :: Int32)
sizeOfDataType GFloat32  = sizeOf (undefined :: Float)
sizeOfDataType GFloat64  = sizeOf (undefined :: Double)
sizeOfDataType GCInt16   = sizeOf (undefined :: (Pair Int16))
sizeOfDataType GCInt32   = sizeOf (undefined :: (Pair Int32))
sizeOfDataType GCFloat32 = sizeOf (undefined :: (Pair Float))
sizeOfDataType GCFloat64 = sizeOf (undefined :: (Pair Double))
sizeOfDataType GUnknown  = error "GDAL.DataType.sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}


hsDataType :: forall a. GDALType a => Proxy a -> DataTypeK
hsDataType _ = dataTypeK (dataType :: DataType a)

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------

instance GDALType Word8 where
  type SizeOf Word8 = 1
  dataType    = GDT_Byte
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  type SizeOf Word16 = 2
  dataType    = GDT_UInt16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  type SizeOf Word32 = 4
  dataType    = GDT_UInt32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  type SizeOf Int16 = 2
  dataType    = GDT_Int16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  type SizeOf Int32 = 4
  dataType    = GDT_Int32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  type SizeOf Float = 4
  dataType    = GDT_Float32
  toCDouble   = realToFrac
  fromCDouble = realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  type SizeOf Double = 8
  dataType    = GDT_Float64
  -- We use coerce to work around https://ghc.haskell.org/trac/ghc/ticket/3676
  toCDouble   = coerce
  fromCDouble = coerce
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Int16) where
  type SizeOf (Pair Int16) = 4
  dataType    = GDT_CInt16
  toCDouble   = fromIntegral . pFst
  fromCDouble = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Int32) where
  type SizeOf (Pair Int32) = 8
  dataType    = GDT_CInt32
  toCDouble   = fromIntegral . pFst
  fromCDouble = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Float) where
  type SizeOf (Pair Float) = 8
  dataType    = GDT_CFloat32
  toCDouble   = realToFrac . pFst
  fromCDouble = (:+: 0) . realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Double) where
  type SizeOf (Pair Double) = 16
  dataType    = GDT_CFloat64
  toCDouble   = coerce . pFst
  fromCDouble = (:+: 0) . coerce
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}
