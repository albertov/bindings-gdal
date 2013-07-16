{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Bindings.GDAL.Internal (
    Datatype (..)
  , Access (..)
  , ColorInterpretation (..)
  , PaletteInterpretation (..)
  , Error (..)
  , Geotransform (..)
  , ProgressFun

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
  , open
  , openShared
  , createCopy'
  , createCopy

  , datatypeSize
  , datatypeByName
  , datatypeUnion
  , datatypeIsComplex

  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform

  , withRasterBand
  , bandDatatype
  , blockSize
  , bandSize
  , readBand
  , writeBand
  , fillBand

) where

import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Monad (liftM, foldM)

import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0, unsafeToForeignPtr0)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

import System.IO.Unsafe (unsafePerformIO)

#include "gdal.h"
#include "cpl_string.h"
#include "cpl_error.h"

{# enum CPLErr as Error {upcaseFirstLetter} deriving (Eq,Show) #}

{# enum GDALDataType as Datatype {upcaseFirstLetter} deriving (Eq) #}
instance Show Datatype where
   show = getDatatypeName

{# fun pure unsafe GDALGetDataTypeSize as datatypeSize
    { fromEnumC `Datatype' } -> `Int' #}

{# fun pure unsafe GDALDataTypeIsComplex as datatypeIsComplex
    { fromEnumC `Datatype' } -> `Bool' #}

{# fun pure unsafe GDALGetDataTypeName as getDatatypeName
    { fromEnumC `Datatype' } -> `String' #}

{# fun pure unsafe GDALGetDataTypeByName as datatypeByName
    { `String' } -> `Datatype' toEnumC #}

{# fun pure unsafe GDALDataTypeUnion as datatypeUnion
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

{# fun GDALOpen as open
    { `String', fromEnumC `Access'} -> `Maybe Dataset' newDatasetHandle* #}

{# fun GDALOpen as openShared
    { `String', fromEnumC `Access'} -> `Maybe Dataset' newDatasetHandle* #}

createCopy' :: Driver -> String -> Dataset -> Bool -> DriverOptions
            -> ProgressFun -> IO (Maybe Dataset)
createCopy' driver path dataset strict options progressFun
  = withCString path $ \p ->
    withDataset dataset $ \ds -> do
    let s = if strict then 1 else 0
        o = toOptionList options
    pFunc <- wrapProgressFun progressFun
    ptr <- {#call GDALCreateCopy as ^#} driver p ds s o pFunc (castPtr nullPtr)
    freeHaskellFunPtr pFunc 
    newDatasetHandle ptr

createCopy :: Driver -> String -> Dataset -> Bool -> DriverOptions
           -> IO (Maybe Dataset)
createCopy driver path dataset strict options
  = createCopy' driver path dataset strict options (\_ _ _ -> return 1)

type ProgressFun = CDouble -> Ptr CChar -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  wrapProgressFun :: ProgressFun -> IO (FunPtr ProgressFun)


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

{# fun pure unsafe GDALGetProjectionRef as datasetProjection
    {  withDataset*  `Dataset'} -> `String' #}

{# fun unsafe GDALSetProjection as setDatasetProjection
    {  withDataset*  `Dataset', `String'} -> `Error' toEnumC #}

data Geotransform = Geotransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

datasetGeotransform :: Dataset -> Maybe Geotransform
datasetGeotransform ds = unsafePerformIO $ withDataset ds $ \dPtr -> do
    allocaArray 6 $ \a -> do
      let err = {#call pure unsafe GDALGetGeoTransform as ^#} dPtr a
      case toEnumC err of
           CE_None -> liftM Just $ Geotransform
                       <$> liftM realToFrac (peekElemOff a 0)
                       <*> liftM realToFrac (peekElemOff a 1)
                       <*> liftM realToFrac (peekElemOff a 2)
                       <*> liftM realToFrac (peekElemOff a 3)
                       <*> liftM realToFrac (peekElemOff a 4)
                       <*> liftM realToFrac (peekElemOff a 5)
           _       -> return Nothing

setDatasetGeotransform :: Dataset -> Geotransform -> IO (Error)
setDatasetGeotransform ds gt = withDataset ds $ \dPtr -> do
    allocaArray 6 $ \a -> do
        let (Geotransform g0 g1 g2 g3 g4 g5) = gt
        pokeElemOff a 0 (realToFrac g0)
        pokeElemOff a 1 (realToFrac g1)
        pokeElemOff a 2 (realToFrac g2)
        pokeElemOff a 3 (realToFrac g3)
        pokeElemOff a 4 (realToFrac g4)
        pokeElemOff a 5 (realToFrac g5)
        liftM toEnumC $ {#call unsafe GDALSetGeoTransform as ^#} dPtr a

withRasterBand :: Dataset -> Int -> (Maybe RasterBand -> IO a) -> IO a
withRasterBand ds band f = withDataset ds $ \dPtr -> do
    rBand@(RasterBand p) <- {# call unsafe GDALGetRasterBand as ^ #}
                              dPtr (fromIntegral band)
    f (if p == nullPtr then Nothing else Just rBand)

{# fun pure unsafe GDALGetRasterDataType as bandDatatype
   { id `RasterBand'} -> `Datatype' toEnumC #}

blockSize :: RasterBand -> (Int,Int)
blockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr ->
   {#call unsafe GDALGetBlockSize as ^#} band xPtr yPtr >>
   liftA2 (,) (liftM fromIntegral $ peek xPtr) (liftM fromIntegral $ peek yPtr)

bandSize :: RasterBand -> (Int, Int)
bandSize band
  = ( fromIntegral . {# call pure unsafe GDALGetRasterBandXSize as ^#} $ band
    , fromIntegral . {# call pure unsafe GDALGetRasterBandYSize as ^#} $ band
    )

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
    fp <- mallocForeignPtrArray (bx * by)
    err <- withForeignPtr fp $ \ptr -> do
        {#call GDALRasterAdviseRead as ^#}
          band
          (fromIntegral xoff)
          (fromIntegral yoff)
          (fromIntegral sx)
          (fromIntegral sy)
          (fromIntegral bx)
          (fromIntegral by)
          (fromEnumC (datatype ptr))
          (castPtr nullPtr)
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
         CE_None -> return $ Just $ unsafeFromForeignPtr0 fp (bx * by)
         _       -> return Nothing
        
writeBand :: (Storable a, HasDatatype (Ptr a))
  => RasterBand
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Vector a
  -> IO (Error)
writeBand band xoff yoff sx sy bx by pxs lns vec = do
    let nElems    = bx * by
        (fp, len) = unsafeToForeignPtr0 vec
    if nElems /= len
      then return CE_Failure
      else withForeignPtr fp $ \ptr -> liftM toEnumC $
          {#call GDALRasterIO as ^#}
            band
            (fromEnumC GF_Write) 
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


fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> Ptr CString
toOptionList opts =  unsafePerformIO $ foldM folder nullPtr opts
  where folder acc (k,v) = withCString k $ \k' -> withCString v $ \v' ->
                           {#call unsafe CSLSetNameValue as ^#} acc k' v'
