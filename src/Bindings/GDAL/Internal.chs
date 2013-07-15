{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Bindings.GDAL.Internal (
    Datatype (..)
  , Access (..)
  , RwFlag (..)
  , ColorInterpretation (..)
  , PaletteInterpretation (..)
  , Error (..)

  , DriverOptions

  , MajorObject
  , Dataset
  , RasterBand
  , Driver
  , ColorTable
  , RasterAttributeTable

  , driverByName
  , create
  , createMem
  , flushCache

  , getDatatypeSize
  , getDatatypeByName
  , dataTypeUnion
  , dataTypeIsComplex

  , withRasterBand
  , getRasterDatatype

  , readBand
  , fillBand

) where

import Control.Monad

import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Utils

import System.IO.Unsafe

#include "gdal.h"
#include "cpl_string.h"
#include "cpl_error.h"

{# enum CPLErr as Error {upcaseFirstLetter} deriving (Eq,Show) #}

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
    { id `Driver', `String', `Int', `Int', `Int', fromEnumC `Datatype',
      toOptionList `DriverOptions' }
    -> `Maybe Dataset' newDatasetHandle* #}

newDatasetHandle :: Ptr Dataset -> IO (Maybe Dataset)
newDatasetHandle p =
    if p==nullPtr then return Nothing
    else newForeignPtr closeDataset p >>= (return . Just . Dataset)

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset) -> IO ())

createMem:: Int -> Int -> Int -> Datatype -> DriverOptions -> IO (Maybe Dataset)
createMem = case driverByName "MEM" of
                 Nothing -> (\_ _ _ _ _ -> return Nothing)
                 Just d  -> create d ""

{# fun GDALFlushCache as flushCache
    {  withDataset*  `Dataset'} -> `()' #}


withRasterBand :: Dataset -> Int -> (Maybe RasterBand -> IO a) -> IO a
withRasterBand ds band f = withDataset ds $ \dPtr -> do
    rBand@(RasterBand p) <- {# call unsafe GDALGetRasterBand as ^ #}
                              dPtr (fromIntegral band)
    f (if p == nullPtr then Nothing else Just rBand)

{# fun pure unsafe GDALGetRasterDataType as getRasterDatatype
   { id `RasterBand'} -> `Datatype' toEnumC #}

{# fun GDALFillRaster as fillBand
    { id `RasterBand', `Double', `Double'} -> `Error' toEnumC #}

class HasDatatype a where
    datatype :: a -> Datatype

instance HasDatatype (Ptr Word8)  where datatype _ = GDT_Byte
instance HasDatatype (Ptr Word16) where datatype _ = GDT_UInt16
instance HasDatatype (Ptr Word32) where datatype _ = GDT_UInt32
instance HasDatatype (Ptr Int16)  where datatype _ = GDT_Int16
instance HasDatatype (Ptr Int32)  where datatype _ = GDT_Int32
instance HasDatatype (Ptr Float)  where datatype _ = GDT_Float32
instance HasDatatype (Ptr Double) where datatype _ = GDT_Float64

readBand :: (Storable a, HasDatatype (Ptr a))
  => RasterBand
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> IO (Maybe (Vector a))
readBand band xoff yoff sx sy bx by pxs lns = do
    let nElems = bx * by
    fp <- mallocForeignPtrArray nElems
    err <- withForeignPtr fp $ \ptr -> do
        {#call GDALRasterIO as ^#}
          band
          (fromEnumC GF_Read) 
          (fromIntegral xoff)
          (fromIntegral yoff)
          (fromIntegral sx)
          (fromIntegral sy)
          (castPtr ptr)
          (fromIntegral bx)
          (fromIntegral by)
          (fromEnumC (datatype ptr))
          (fromIntegral pxs)
          (fromIntegral lns)
    case toEnumC err of
         CE_None -> return $ Just $ unsafeFromForeignPtr0 fp nElems
         _       -> return Nothing
        


fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> Ptr CString
toOptionList opts =  unsafePerformIO $ foldM folder nullPtr opts
  where folder acc (k,v) = setNameValue acc k v

{# fun CSLSetNameValue as setNameValue
   { id `Ptr CString', `String', `String'} -> `Ptr CString' id  #}
