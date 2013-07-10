{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.GDAL.Internal where

import Control.Monad

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils

import System.IO.Unsafe

#include "gdal.h"
#include "cpl_string.h"

{# enum GDALDataType as DataType {upcaseFirstLetter} deriving (Eq) #}
instance Show DataType where
   show = getDataTypeName

{# fun pure GDALGetDataTypeSize as getDataTypeSize
    { fromEnumC `DataType' } -> `Int' #}

{# fun pure GDALDataTypeIsComplex as dataTypeIsComplex
    { fromEnumC `DataType' } -> `Bool' #}

{# fun pure GDALGetDataTypeName as getDataTypeName
    { fromEnumC `DataType' } -> `String' #}

{# fun pure GDALGetDataTypeByName as getDataTypeByName
    { `String' } -> `DataType' toEnumC #}

{# fun pure GDALDataTypeUnion as dataTypeUnion
    { fromEnumC `DataType', fromEnumC `DataType' } -> `DataType' toEnumC #}


{# enum GDALAccess as Access {upcaseFirstLetter} deriving (Eq, Show) #}

{# enum GDALRWFlag as RwFlag {upcaseFirstLetter} deriving (Eq, Show) #}

{# enum GDALColorInterp as ColorInterpretation {upcaseFirstLetter}
   deriving (Eq) #}

instance Show ColorInterpretation where
    show = getColorInterpretationName

{# fun pure GDALGetColorInterpretationName as getColorInterpretationName
    { fromEnumC `ColorInterpretation' } -> `String' #}

{# fun pure GDALGetColorInterpretationByName as getColorInterpretationByName
    { `String' } -> `ColorInterpretation' toEnumC #}


{# enum GDALPaletteInterp as PaletteInterpretation {upcaseFirstLetter}
   deriving (Eq) #}

instance Show PaletteInterpretation where
    show = getPaletteInterpretationName

{# fun pure GDALGetPaletteInterpretationName as getPaletteInterpretationName
    { fromEnumC `PaletteInterpretation' } -> `String' #}


{#pointer GDALMajorObjectH as MajorObject newtype#}
{#pointer GDALDatasetH as Dataset newtype#}
{#pointer GDALRasterBandH as RasterBand newtype#}
{#pointer GDALDriverH as Driver newtype#}
{#pointer GDALColorTableH as ColorTable newtype#}
{#pointer GDALRasterAttributeTableH as RasterAttributeTable newtype#}

{# fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{# fun GDALCreate as create
    { id `Driver', `String', `Int', `Int', `Int', fromEnumC `DataType',
      toOptionList `[(String,String)]' }
    -> `Dataset' id #}

{# fun GDALFlushCache as flush
    { id `Dataset'} -> `()' #}

{# fun GDALClose as close
    { id `Dataset'} -> `()' #}

{# fun pure GDALGetDriverByName as _driverByName
    { `String' } -> `Driver' id #}


driverByName s = (unsafePerformIO $ registerAllDrivers) `seq` _driverByName s

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> Ptr CString
toOptionList opts =  unsafePerformIO $ foldM folder nullPtr opts
  where folder acc (k,v) = setNameValue acc k v

{# fun CSLSetNameValue as setNameValue
   { id `Ptr CString', `String', `String'} -> `Ptr CString' id  #}
