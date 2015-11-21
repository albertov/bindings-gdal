{-# LANGUAGE DataKinds #-}
module GDAL.Internal.DataType.Internal (
    DataTypeK (..)
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
  ) where

#include "gdal.h"

import GDAL.Internal.Util (fromEnumC)

import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Ptr

import System.IO.Unsafe (unsafePerformIO)


{#enum GDALDataType as DataTypeK {}
  omit (GDT_TypeCount)
  with prefix = "GDT_"
  add  prefix = "G"
  deriving (Eq,Ord,Bounded) #}

instance Show DataTypeK where
  show d = unsafePerformIO $
    {#call unsafe GDALGetDataTypeName as ^#} (fromEnumC d) >>= peekCString

type GDT_Byte = 'GByte
type GDT_UInt16 = 'GUInt16
type GDT_UInt32 = 'GUInt32
type GDT_Int16 = 'GInt16
type GDT_Int32 = 'GInt32
type GDT_Float32 = 'GFloat32
type GDT_Float64 = 'GFloat64
type GDT_CInt16 = 'GCInt16
type GDT_CInt32 = 'GCInt32
type GDT_CFloat32 = 'GCFloat32
type GDT_CFloat64 = 'GCFloat64
