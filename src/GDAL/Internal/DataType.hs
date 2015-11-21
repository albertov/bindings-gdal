{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Internal.DataType (
    DataType (..)
  , GDALType (..)
  , DataTypeK (..)
  , HsType
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
  , hsDataTypeK
) where

import GDAL.Internal.Types (Pair(..), pFst)
import GDAL.Internal.DataType.Internal
import Data.Word
import Data.Int

import Foreign.Storable (Storable(sizeOf))
import Foreign.C.Types (CDouble(..))


data DataType :: DataTypeK -> * where
  GDT_Byte     :: DataType GDT_Byte
  GDT_UInt16   :: DataType GDT_UInt16
  GDT_UInt32   :: DataType GDT_UInt32
  GDT_Int16    :: DataType GDT_Int16
  GDT_Int32    :: DataType GDT_Int32
  GDT_Float32  :: DataType GDT_Float32
  GDT_Float64  :: DataType GDT_Float64
  GDT_CInt16   :: DataType GDT_CInt16
  GDT_CInt32   :: DataType GDT_CInt32
  GDT_CFloat32 :: DataType GDT_CFloat32
  GDT_CFloat64 :: DataType GDT_CFloat64

instance Show (DataType a) where
  show = show . dataTypeK

instance Enum (DataType a) where
  fromEnum = fromEnum . dataTypeK
  toEnum   = error "toEnum (DataType a) is not implemented"

dataTypeK :: DataType a -> DataTypeK
dataTypeK GDT_Byte = GByte
dataTypeK GDT_UInt16 = GUInt16
dataTypeK GDT_UInt32 = GUInt32
dataTypeK GDT_Int16 = GInt16
dataTypeK GDT_Int32 = GInt32
dataTypeK GDT_Float32 = GFloat32
dataTypeK GDT_Float64 = GFloat64
dataTypeK GDT_CInt16 = GCInt16
dataTypeK GDT_CInt32 = GCInt32
dataTypeK GDT_CFloat32 = GCFloat32
dataTypeK GDT_CFloat64 = GCFloat64

reifyDataTypeK
  :: DataTypeK
  -> (forall d. GDALType (HsType d) => DataType d -> b)
  -> b
reifyDataTypeK GByte     f = f GDT_Byte
reifyDataTypeK GUInt16   f = f GDT_UInt16
reifyDataTypeK GUInt32   f = f GDT_UInt32
reifyDataTypeK GInt16    f = f GDT_Int16
reifyDataTypeK GInt32    f = f GDT_Int32
reifyDataTypeK GFloat32  f = f GDT_Float32
reifyDataTypeK GFloat64  f = f GDT_Float64
reifyDataTypeK GCInt16   f = f GDT_CInt16
reifyDataTypeK GCInt32   f = f GDT_CInt32
reifyDataTypeK GCFloat32 f = f GDT_CFloat32
reifyDataTypeK GCFloat64 f = f GDT_CFloat64
reifyDataTypeK GUnknown  _ = error "GDAL.DataType.reifyDataTypeK: GDT_Unknown"

hsType :: GDALType (HsType d) => DataType d -> HsType d
hsType = const undefined

hsDataTypeK :: GDALType a => a -> DataTypeK
hsDataTypeK = dataTypeK . dataType
{-# INLINE hsDataTypeK #-}

class ( Storable a
      , Eq a
      , Show a
      , Num a
      , HsType (TypeK a) ~ a
      ) => GDALType a where
  type TypeK a  :: DataTypeK
  dataType      :: a -> DataType (TypeK a)
  toCDouble     :: a -> CDouble
  fromCDouble   :: CDouble -> a


type family HsType (a :: DataTypeK) where
  HsType GDT_Byte = Word8
  HsType GDT_UInt16 = Word16
  HsType GDT_UInt32 = Word32
  HsType GDT_Int16 = Int16
  HsType GDT_Int32 = Int32
  HsType GDT_Float32 = Float
  HsType GDT_Float64 = Double
  HsType GDT_CInt16 = Pair Int16
  HsType GDT_CInt32 = Pair Int32
  HsType GDT_CFloat32 = Pair Float
  HsType GDT_CFloat64 = Pair Double

sizeOfDataType :: DataTypeK -> Int
sizeOfDataType dt = reifyDataTypeK dt (sizeOf . hsType)
{-# INLINE sizeOfDataType #-}

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------

instance GDALType Word8 where
  type TypeK Word8 = GDT_Byte
  dataType _  = GDT_Byte
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  type TypeK Word16 = GDT_UInt16
  dataType _  = GDT_UInt16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  type TypeK Word32 = GDT_UInt32
  dataType _  = GDT_UInt32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  type TypeK Int16 = GDT_Int16
  dataType _  = GDT_Int16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  type TypeK Int32 = GDT_Int32
  dataType _  = GDT_Int32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  type TypeK Float = GDT_Float32
  dataType _  = GDT_Float32
  toCDouble   = realToFrac
  fromCDouble = realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  type TypeK Double = GDT_Float64
  dataType _  = GDT_Float64
  toCDouble   = realToFrac
  fromCDouble = realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Int16) where
  type TypeK (Pair Int16) = GDT_CInt16
  dataType _  = GDT_CInt16
  toCDouble   = fromIntegral . pFst
  fromCDouble = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}


instance GDALType (Pair Int32) where
  type TypeK (Pair Int32) = GDT_CInt32
  dataType _  = GDT_CInt32
  toCDouble   = fromIntegral . pFst
  fromCDouble = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Float) where
  type TypeK (Pair Float) = GDT_CFloat32
  dataType _  = GDT_CFloat32
  toCDouble   = realToFrac . pFst
  fromCDouble = (:+: 0) . realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Double) where
  type TypeK (Pair Double) = GDT_CFloat64
  dataType _  = GDT_CFloat64
  toCDouble   = realToFrac . pFst
  fromCDouble = (:+: 0) . realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}
