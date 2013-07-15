{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.GDAL.Internal (
    Datatype (..)
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

  , getDatatypeSize
  , getDatatypeByName
  , dataTypeUnion
  , dataTypeIsComplex

  , getRasterBand
  , getRasterDatatype
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

{# enum GDALDataType as Datatype {upcaseFirstLetter} deriving (Eq) #}
instance Show Datatype where
   show = getDatatypeName

{# fun pure unsafe GDALGetDataTypeSize as getDatatypeSize
    { fromEnumC `Datatype' } -> `Int' #}

{# fun pure unsafe GDALDataTypeIsComplex as dataTypeIsComplex
    { fromEnumC `Datatype' } -> `Bool' #}

{# fun pure unsafe GDALGetDataTypeName as getDatatypeName
    { fromEnumC `Datatype' } -> `String' #}

{# fun pure unsafe GDALGetDataTypeByName as getDatatypeByName
    { `String' } -> `Datatype' toEnumC #}

{# fun pure unsafe GDALDataTypeUnion as dataTypeUnion
    { fromEnumC `Datatype', fromEnumC `Datatype' } -> `Datatype' toEnumC #}


{# enum GDALAccess as Access {upcaseFirstLetter} deriving (Eq, Show) #}

{# enum GDALRWFlag as RwFlag {upcaseFirstLetter} deriving (Eq, Show) #}

{# enum GDALColorInterp as ColorInterpretation {upcaseFirstLetter}
   deriving (Eq) #}

instance Show ColorInterpretation where
    show = getColorInterpretationName

{# fun pure unsafe GDALGetColorInterpretationName as getColorInterpretationName
    { fromEnumC `ColorInterpretation' } -> `String' #}

{# fun pure unsafe GDALGetColorInterpretationByName as getColorInterpretationByName
    { `String' } -> `ColorInterpretation' toEnumC #}


{# enum GDALPaletteInterp as PaletteInterpretation {upcaseFirstLetter}
   deriving (Eq) #}

instance Show PaletteInterpretation where
    show = getPaletteInterpretationName

{# fun pure unsafe GDALGetPaletteInterpretationName as getPaletteInterpretationName
    { fromEnumC `PaletteInterpretation' } -> `String' #}


{#pointer GDALMajorObjectH as MajorObject newtype#}
{#pointer GDALDatasetH as Dataset foreign newtype#}
{#pointer GDALRasterBandH as RasterBand_ newtype#}

newtype RasterBand = RasterBand (RasterBand_, Dataset)

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
    { id `Driver', `String', `Int', `Int', `Int', fromEnumC `Datatype',
      toOptionList `DriverOptions' }
    -> `Maybe Dataset' newDatasetHandle* #}

newDatasetHandle :: Ptr Dataset -> IO (Maybe Dataset)
newDatasetHandle p =
    if p==nullPtr then return Nothing
    else newForeignPtr closeDataset p >>= (return . Just . Dataset)

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset) -> IO ())


{# fun GDALFlushCache as flushCache
    {  withDataset*  `Dataset'} -> `()' #}


getRasterBand :: Dataset -> Int -> IO (Maybe RasterBand)
getRasterBand ds band = withDataset ds $ \dPtr -> do
    rBand@(RasterBand_ p) <- {# call unsafe GDALGetRasterBand as ^ #}
                              dPtr (fromIntegral band)
    return (if p == nullPtr then Nothing else Just $ RasterBand (rBand,ds))

{# fun pure unsafe GDALGetRasterDataType as getRasterDatatype
   { extractBand `RasterBand'} -> `Datatype' toEnumC #}

extractBand :: RasterBand -> RasterBand_
extractBand (RasterBand (rb,_)) = rb

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> Ptr CString
toOptionList opts =  unsafePerformIO $ foldM folder nullPtr opts
  where folder acc (k,v) = setNameValue acc k v

{# fun CSLSetNameValue as setNameValue
   { id `Ptr CString', `String', `String'} -> `Ptr CString' id  #}
