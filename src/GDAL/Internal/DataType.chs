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
  , GDataType (..)
  , GDALType (..)

  , sizeOfDataType
  , dataType
) where

#include "gdal.h"

import GDAL.Internal.Util (fromEnumC)

import Data.Word
import Data.Int
import Data.Complex

import Foreign.Storable (Storable(..))
import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Ptr

import System.IO.Unsafe (unsafePerformIO)


------------------------------------------------------------------------------
-- DataType
------------------------------------------------------------------------------

{#enum GDALDataType as DataType {} omit (GDT_TypeCount)
  deriving (Eq,Ord,Bounded) #}

instance Show DataType where
  show d = unsafePerformIO $
    {#call unsafe GDALGetDataTypeName as ^#} (fromEnumC d) >>= peekCString

data GDataType :: DataType -> * where
  GByte     :: GDataType 'GDT_Byte
  GUInt16   :: GDataType 'GDT_UInt16
  GUInt32   :: GDataType 'GDT_UInt32
  GInt16    :: GDataType 'GDT_Int16
  GInt32    :: GDataType 'GDT_Int32
  GFloat32  :: GDataType 'GDT_Float32
  GFloat64  :: GDataType 'GDT_Float64
  GCInt16   :: GDataType 'GDT_CInt16
  GCInt32   :: GDataType 'GDT_CInt32
  GCFloat32 :: GDataType 'GDT_CFloat32
  GCFloat64 :: GDataType 'GDT_CFloat64

instance Show (GDataType a) where
  show = show . gDataType


gDataType :: GDataType a -> DataType
gDataType GByte = GDT_Byte
gDataType GUInt16 = GDT_UInt16
gDataType GUInt32 = GDT_UInt32
gDataType GInt16 = GDT_Int16
gDataType GInt32 = GDT_Int32
gDataType GFloat32 = GDT_Float32
gDataType GFloat64 = GDT_Float64
gDataType GCInt16 = GDT_CInt16
gDataType GCInt32 = GDT_CInt32
gDataType GCFloat32 = GDT_CFloat32
gDataType GCFloat64 = GDT_CFloat64

unDataType
  :: DataType
  -> (forall d. GDALType (HsType d) => GDataType d -> b)
  -> b
unDataType GDT_Byte     f = f GByte
unDataType GDT_UInt16   f = f GUInt16
unDataType GDT_UInt32   f = f GUInt32
unDataType GDT_Int16    f = f GInt16
unDataType GDT_Int32    f = f GInt32
unDataType GDT_Float32  f = f GFloat32
unDataType GDT_Float64  f = f GFloat64
unDataType GDT_CInt16   f = f GCInt16
unDataType GDT_CInt32   f = f GCInt32
unDataType GDT_CFloat32 f = f GCFloat32
unDataType GDT_CFloat64 f = f GCFloat64
unDataType GDT_Unknown  _ = error "GDAL.DataType.unDataType: GDT_Unknown"

hsType :: GDALType (HsType d) => GDataType d -> HsType d
hsType = const undefined

dataType :: GDALType a => a -> DataType
dataType = gDataType . dataType'
{-# INLINE dataType #-}

class (Storable a, Eq a, HsType (GType a) ~ a) => GDALType a where
  type GType a :: DataType
  dataType'     :: a -> GDataType (GType a)
  toCDouble     :: a -> CDouble
  fromCDouble   :: CDouble -> a


type family HsType (a :: DataType) where
  HsType 'GDT_Byte = Word8
  HsType 'GDT_UInt16 = Word16
  HsType 'GDT_UInt32 = Word32
  HsType 'GDT_Int16 = Int16
  HsType 'GDT_Int32 = Int32
  HsType 'GDT_Float32 = Float
  HsType 'GDT_Float64 = Double
  HsType 'GDT_CInt16 = Complex Int16
  HsType 'GDT_CInt32 = Complex Int32
  HsType 'GDT_CFloat32 = Complex Float
  HsType 'GDT_CFloat64 = Complex Double

sizeOfDataType :: DataType -> Int
sizeOfDataType dt = unDataType dt (sizeOf . hsType)
{-# INLINE sizeOfDataType #-}

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------

instance GDALType Word8 where
  type GType Word8 = 'GDT_Byte
  dataType' _ = GByte
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  type GType Word16 = 'GDT_UInt16
  dataType' _ = GUInt16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  type GType Word32 = 'GDT_UInt32
  dataType' _ = GUInt32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  type GType Int16 = 'GDT_Int16
  dataType' _ = GInt16
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  type GType Int32 = 'GDT_Int32
  dataType' _ = GInt32
  toCDouble   = fromIntegral
  fromCDouble = truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  type GType Float = 'GDT_Float32
  dataType' _ = GFloat32
  toCDouble   = realToFrac
  fromCDouble = realToFrac
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  type GType Double = 'GDT_Float64
  dataType' _ = GFloat64
  toCDouble   = realToFrac
  fromCDouble = realToFrac
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Int16) where
  type GType (Complex Int16) = 'GDT_CInt16
  dataType' _ = GCInt16
  toCDouble   = fromIntegral . realPart
  fromCDouble = (:+ 0) . truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}


instance GDALType (Complex Int32) where
  type GType (Complex Int32) = 'GDT_CInt32
  dataType' _ = GCInt32
  toCDouble   = fromIntegral . realPart
  fromCDouble = (:+ 0) . truncate
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Float) where
  type GType (Complex Float) = 'GDT_CFloat32
  dataType' _ = GCFloat32
  toCDouble   = realToFrac . realPart
  fromCDouble = (:+ 0) . realToFrac
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Double) where
  type GType (Complex Double) = 'GDT_CFloat64
  dataType' _ = GCFloat64
  toCDouble   = realToFrac . realPart
  fromCDouble = (:+ 0) . realToFrac
  {-# INLINE dataType' #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}
