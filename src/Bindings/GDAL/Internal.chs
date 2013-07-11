{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.GDAL.Internal (
    DataType (..)
  , Access (..)
  , RwFlag (..)
  , ColorInterpretation (..)
  , PaletteInterpretation (..)

  , DriverOptions

  , MajorObject
  , Dataset
  , RasterBand
  , Driver
  , ColorTable
  , RasterAttributeTable

  , driverByName
  , create
  , flushCache

  , getDataTypeSize
  , getDataTypeByName
  , dataTypeUnion
  , dataTypeIsComplex
) where

import Control.Monad

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
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
{#pointer GDALDatasetH as Dataset foreign newtype#}
{#pointer GDALRasterBandH as RasterBand newtype#}
{#pointer GDALDriverH as Driver newtype#}
{#pointer GDALColorTableH as ColorTable newtype#}
{#pointer GDALRasterAttributeTableH as RasterAttributeTable newtype#}

{# fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{# fun GDALGetDriverByName as c_driverByName
    { `String' } -> `Driver' id #}

driverByName :: String -> Maybe Driver
driverByName s = unsafePerformIO $ do
    registerAllDrivers
    driver@(Driver ptr) <- c_driverByName s
    return $ if ptr==nullPtr then Nothing else Just driver


type DriverOptions = [(String,String)]

{# fun GDALCreate as create
    { id `Driver', `String', `Int', `Int', `Int', fromEnumC `DataType',
      toOptionList `DriverOptions' }
    -> `Maybe Dataset' newDatasetHandle* #}

newDatasetHandle :: Ptr Dataset -> IO (Maybe Dataset)
newDatasetHandle p =
    if p==nullPtr
    then return Nothing
    else do
        fp <- newForeignPtr closeDataset p
        return $ Just $ Dataset fp

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset) -> IO ())


{# fun GDALFlushCache as flushCache
    {  withDataset*  `Dataset'} -> `()' #}


fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> Ptr CString
toOptionList opts =  unsafePerformIO $ foldM folder nullPtr opts
  where folder acc (k,v) = setNameValue acc k v

{# fun CSLSetNameValue as setNameValue
   { id `Ptr CString', `String', `String'} -> `Ptr CString' id  #}
