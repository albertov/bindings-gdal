{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Bindings.GDAL.Internal (
    Datatype (..)
  , ColorInterpretation (..)
  , PaletteInterpretation (..)
  , GDALException
  , isGDALException
  , Geotransform (..)
  , MaybeIOVector
  , DriverOptions
  , MajorObject
  , Dataset
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , Driver
  , ColorTable
  , RasterAttributeTable

  , setQuietErrorHandler

  , registerAllDrivers
  , driverByName
  , create
  , create'
  , createMem
  , flushCache
  , openReadOnly
  , openReadWrite
  , createCopy

  , datatypeSize
  , datatypeByName
  , datatypeUnion
  , datatypeIsComplex

  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetBandCount

  , withBand
  , bandDatatype
  , bandBlockSize
  , bandBlockLen
  , bandSize
  , bandNodataValue
  , setBandNodataValue
  , readBand
  , readBandBlock
  , writeBand
  , writeBandBlock
  , fillBand

) where

import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Concurrent (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (finally, bracket, throw, Exception(..), SomeException)
import Control.Monad (liftM, foldM)

import Data.Int (Int16, Int32)
import Data.Complex (Complex(..), realPart, imagPart)
import Data.Maybe (isJust)
import Data.Typeable (Typeable, cast, typeOf)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0, unsafeToForeignPtr0)
import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, freeHaskellFunPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr
                          ,mallocForeignPtrArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (toBool, fromBool)

import System.IO.Unsafe (unsafePerformIO)

#include "gdal.h"
#include "cpl_string.h"
#include "cpl_error.h"

data GDALException = GDALException Error String
     deriving (Show, Typeable)

instance Exception GDALException

isGDALException :: SomeException -> Bool
isGDALException e = isJust (fromException e :: Maybe GDALException)

{# enum CPLErr as Error {upcaseFirstLetter} deriving (Eq,Show) #}


type ErrorHandler = CInt -> CInt -> CString -> IO ()

foreign import ccall "cpl_error.h &CPLQuietErrorHandler"
  c_quietErrorHandler :: FunPtr ErrorHandler

foreign import ccall "cpl_error.h CPLSetErrorHandler"
  c_setErrorHandler :: FunPtr ErrorHandler -> IO (FunPtr ErrorHandler)

setQuietErrorHandler :: IO ()
setQuietErrorHandler = setErrorHandler c_quietErrorHandler

setErrorHandler :: FunPtr ErrorHandler -> IO ()
setErrorHandler h = c_setErrorHandler h >>= (\_->return ())


throwIfError :: String -> IO CInt -> IO ()
throwIfError msg act = do
    e <- liftM toEnumC act
    case e of
      CE_None -> return ()
      e'      -> throw $ GDALException e' msg

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
{#pointer GDALDatasetH as Dataset foreign newtype nocode#}

data ReadOnly
data ReadWrite
newtype Dataset a = Dataset (ForeignPtr (Dataset a), Mutex)

type RODataset = Dataset ReadOnly
type RWDataset = Dataset ReadWrite

withDataset, withDataset' :: (Dataset a) -> (Ptr (Dataset a) -> IO b) -> IO b
withDataset ds@(Dataset (_, m)) fun = withMutex m $ withDataset' ds fun

withDataset' (Dataset (fptr,_)) = withForeignPtr fptr

{#pointer GDALRasterBandH as Band newtype nocode#}
newtype Band a = Band (Ptr (Band a))

type ROBand = Band ReadOnly
type RWBand = Band ReadWrite

{#pointer GDALDriverH as Driver newtype#}
{#pointer GDALColorTableH as ColorTable newtype#}
{#pointer GDALRasterAttributeTableH as RasterAttributeTable newtype#}

{# fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{# fun unsafe GDALGetDriverByName as c_driverByName
    { `String' } -> `Driver' id #}

driverByName :: String -> IO Driver
driverByName s = do
    driver@(Driver ptr) <- c_driverByName s
    if ptr==nullPtr
        then throw $ GDALException CE_Failure "failed to load driver"
        else return driver

type DriverOptions = [(String,String)]

create :: String -> String -> Int -> Int -> Int -> Datatype -> DriverOptions
       -> IO RWDataset
create drv path nx ny bands dtype options = do
    d <- driverByName  drv
    create' d path nx ny bands dtype options

create' :: Driver -> String -> Int -> Int -> Int -> Datatype -> DriverOptions
       -> IO RWDataset
create' drv path nx ny bands dtype options = withCString path $ \path' -> do
    opts <- toOptionList options
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC dtype
    create_ drv path' nx' ny' bands' dtype' opts >>= newDatasetHandle

foreign import ccall safe "gdal.h GDALCreate" create_
  :: Driver
  -> CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CString
  -> IO (Ptr RWDataset)

openReadOnly :: String -> IO RODataset
openReadOnly p = withCString p openIt
  where openIt p' = open_ p' (fromEnumC GA_ReadOnly) >>= newDatasetHandle

openReadWrite :: String -> IO RWDataset
openReadWrite p = withCString p openIt
  where openIt p' = open_ p' (fromEnumC GA_Update) >>= newDatasetHandle

foreign import ccall safe "gdal.h GDALOpen" open_
   :: CString -> CInt -> IO (Ptr (Dataset a))


createCopy' :: Driver -> String -> (Dataset a) -> Bool -> DriverOptions
            -> ProgressFun -> IO (Dataset a)
createCopy' driver path dataset strict options progressFun
  = withCString path $ \p ->
    withDataset dataset $ \ds ->
    withProgressFun progressFun $ \pFunc -> do
        let s = fromBool strict
        o <- toOptionList options
        createCopy_ driver p ds s o pFunc (castPtr nullPtr) >>= newDatasetHandle

foreign import ccall safe "gdal.h GDALCreateCopy" createCopy_
  :: Driver
  -> Ptr CChar
  -> Ptr (Dataset a)
  -> CInt
  -> Ptr CString
  -> FunPtr ProgressFun
  -> Ptr ()
  -> IO (Ptr (Dataset a))


withProgressFun = withCCallback wrapProgressFun

withCCallback w f = bracket (w f) freeHaskellFunPtr

createCopy :: String -> String -> (Dataset a) -> Bool -> DriverOptions
           -> IO (Dataset a)
createCopy driver path dataset strict options = do
  d <- driverByName driver
  createCopy' d path dataset strict options (\_ _ _ -> return 1)

type ProgressFun = CDouble -> Ptr CChar -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  wrapProgressFun :: ProgressFun -> IO (FunPtr ProgressFun)


newDatasetHandle :: Ptr (Dataset a) -> IO (Dataset a)
newDatasetHandle p =
    if p==nullPtr
        then throw $ GDALException CE_Failure "Could not create dataset"
        else do fp <- newForeignPtr closeDataset p
                mutex <- newMutex
                return $ Dataset (fp, mutex)

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset a) -> IO ())

createMem:: Int -> Int -> Int -> Datatype -> DriverOptions -> IO RWDataset
createMem = create "MEM" ""

flushCache d = withDataset d flushCache'

foreign import ccall safe "gdal.h GDALFlushCache" flushCache'
  :: Ptr (Dataset a) -> IO ()


datasetProjection :: Dataset a -> IO String
datasetProjection d = withDataset d $ \d' -> do
    p <- getProjection_ d'
    peekCString p

foreign import ccall unsafe "gdal.h GDALGetProjectionRef" getProjection_
  :: Ptr (Dataset a) -> IO (Ptr CChar)


setDatasetProjection :: RWDataset -> String -> IO ()
setDatasetProjection d p = throwIfError "could not set projection" f
  where f = withDataset d $ \d' -> withCString p $ \p' -> setProjection' d' p'

foreign import ccall unsafe "gdal.h GDALSetProjection" setProjection'
  :: Ptr (Dataset a) -> Ptr CChar -> IO CInt


data Geotransform = Geotransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

datasetGeotransform :: Dataset a -> IO Geotransform
datasetGeotransform ds = withDataset' ds $ \dPtr -> do
    allocaArray 6 $ \a -> do
      throwIfError "could not get geotransform" $ getGeoTransform dPtr a
      Geotransform <$> liftM realToFrac (peekElemOff a 0)
                   <*> liftM realToFrac (peekElemOff a 1)
                   <*> liftM realToFrac (peekElemOff a 2)
                   <*> liftM realToFrac (peekElemOff a 3)
                   <*> liftM realToFrac (peekElemOff a 4)
                   <*> liftM realToFrac (peekElemOff a 5)

foreign import ccall unsafe "gdal.h GDALGetGeoTransform" getGeoTransform
  :: Ptr (Dataset a) -> Ptr CDouble -> IO CInt

setDatasetGeotransform :: RWDataset -> Geotransform -> IO ()
setDatasetGeotransform ds gt = withDataset ds $ \dPtr -> do
    allocaArray 6 $ \a -> do
        let (Geotransform g0 g1 g2 g3 g4 g5) = gt
        pokeElemOff a 0 (realToFrac g0)
        pokeElemOff a 1 (realToFrac g1)
        pokeElemOff a 2 (realToFrac g2)
        pokeElemOff a 3 (realToFrac g3)
        pokeElemOff a 4 (realToFrac g4)
        pokeElemOff a 5 (realToFrac g5)
        throwIfError "could not set geotransform" $ setGeoTransform dPtr a

foreign import ccall unsafe "gdal.h GDALSetGeoTransform" setGeoTransform
  :: Ptr (Dataset a) -> Ptr CDouble -> IO CInt

datasetBandCount :: Dataset a -> IO (Int)
datasetBandCount d = liftM fromIntegral $ withDataset d bandCount_

foreign import ccall unsafe "gdal.h GDALGetRasterCount" bandCount_
  :: Ptr (Dataset a) -> IO CInt

withBand :: Dataset a -> Int -> ((Band a) -> IO b) -> IO b
withBand ds band f = withDataset ds $ \dPtr -> do
    rBand@(Band p) <- getRasterBand dPtr (fromIntegral band)
    if p == nullPtr
        then throw $
            GDALException CE_Failure ("could not get band #" ++ show band)
        else f rBand

foreign import ccall unsafe "gdal.h GDALGetRasterBand" getRasterBand
  :: Ptr (Dataset a) -> CInt -> IO (Band a)


bandDatatype :: Band a -> Datatype
bandDatatype band = toEnumC . getDatatype_ $ band

foreign import ccall unsafe "gdal.h GDALGetRasterDataType" getDatatype_
  :: Band a -> CInt


bandBlockSize :: Band a -> (Int,Int)
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   -- unsafePerformIO is safe here since the block size cant change once
   -- a dataset is created
   getBlockSize_ band xPtr yPtr
   liftA2 (,) (liftM fromIntegral $ peek xPtr) (liftM fromIntegral $ peek yPtr)

foreign import ccall unsafe "gdal.h GDALGetBlockSize" getBlockSize_
    :: Band a -> Ptr CInt -> Ptr CInt -> IO ()


bandBlockLen :: Band a -> Int
bandBlockLen = uncurry (*) . bandBlockSize

bandSize :: Band a -> (Int, Int)
bandSize band
  = (fromIntegral . getXSize_ $ band, fromIntegral . getYSize_ $ band)

foreign import ccall unsafe "gdal.h GDALGetRasterBandXSize" getXSize_
  :: Band a -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterBandYSize" getYSize_
  :: Band a -> CInt


bandNodataValue :: Band a -> IO (Maybe Double)
bandNodataValue b = alloca $ \p -> do
   value <- liftM realToFrac $ getNodata_ b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
   
foreign import ccall unsafe "gdal.h GDALGetRasterNoDataValue" getNodata_
   :: Band a -> Ptr CInt -> IO CDouble
   

setBandNodataValue :: RWBand -> Double -> IO ()
setBandNodataValue b v = throwIfError "could not set nodata" $
                            setNodata_ b (realToFrac v)

foreign import ccall safe "gdal.h GDALSetRasterNoDataValue" setNodata_
    :: RWBand -> CDouble -> IO CInt


fillBand :: RWBand -> Double -> Double -> IO ()
fillBand b r i = throwIfError "could not fill band" $
    fillRaster_ b (realToFrac r) (realToFrac i)

foreign import ccall safe "gdal.h GDALFillRaster" fillRaster_
    :: RWBand -> CDouble -> CDouble -> IO CInt


class HasDatatype a where
    datatype :: a -> Datatype

instance HasDatatype (Ptr Word8)  where datatype _ = GDT_Byte
instance HasDatatype (Ptr Word16) where datatype _ = GDT_UInt16
instance HasDatatype (Ptr Word32) where datatype _ = GDT_UInt32
instance HasDatatype (Ptr Int16)  where datatype _ = GDT_Int16
instance HasDatatype (Ptr Int32)  where datatype _ = GDT_Int32
instance HasDatatype (Ptr Float)  where datatype _ = GDT_Float32
instance HasDatatype (Ptr Double) where datatype _ = GDT_Float64
-- GDT_CInt16 or GDT_CInt32 can be written as Complex (Float|Double) but
-- will be truncated by GDAL. Both can be read as Complex (Float|Double).
-- This is a limitation imposed by Complex a which constrains a to be a
-- RealFloat.
instance HasDatatype (Ptr (Complex Float)) where datatype _ = GDT_CFloat32
instance HasDatatype (Ptr (Complex Double)) where datatype _ = GDT_CFloat64


instance (RealFloat a, Storable a) => Storable (Complex a) where
  sizeOf _ = sizeOf (undefined :: a) * 2
  alignment _ = alignment (undefined :: a)
 
  {-# SPECIALIZE INLINE peek :: Ptr (Complex Float) -> IO (Complex Float) #-}
  {-# SPECIALIZE INLINE peek :: Ptr (Complex Double) -> IO (Complex Double) #-}
  peek p = (:+) <$> peekElemOff (castPtr p) 0 <*> peekElemOff (castPtr p) 1

  {-# SPECIALIZE INLINE
      poke :: Ptr (Complex Float) -> Complex Float -> IO () #-}
  {-# SPECIALIZE INLINE
      poke :: Ptr (Complex Double) -> Complex Double -> IO () #-}
  poke p v = pokeElemOff (castPtr p) 0 (realPart v) >>
             pokeElemOff (castPtr p) 1 (imagPart v)



readBand :: (Storable a, HasDatatype (Ptr a))
  => Band b
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> IO (Vector a)
readBand band xoff yoff sx sy bx by pxs lns = do
    fp <- mallocForeignPtrArray (bx * by)
    withForeignPtr fp $ \ptr -> do
      throwIfError "could not advise read" $
        adviseRead_
          band
          (fromIntegral xoff)
          (fromIntegral yoff)
          (fromIntegral sx)
          (fromIntegral sy)
          (fromIntegral bx)
          (fromIntegral by)
          (fromEnumC (datatype ptr))
          (castPtr nullPtr)
      throwIfError "could not read band" $
        rasterIO_
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
    return $ unsafeFromForeignPtr0 fp (bx * by)

foreign import ccall safe "gdal.h GDALRasterAdviseRead" adviseRead_
    :: Band a -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
    -> Ptr (Ptr CChar) -> IO CInt

        
writeBand :: (Storable a, HasDatatype (Ptr a))
  => RWBand
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Vector a
  -> IO ()
writeBand band xoff yoff sx sy bx by pxs lns vec = do
    let nElems    = bx * by
        (fp, len) = unsafeToForeignPtr0 vec
    if nElems /= len
      then throw $ GDALException CE_Failure "vector of wrong size"
      else withForeignPtr fp $ \ptr -> do
        throwIfError "could not write band" $
          rasterIO_
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

foreign import ccall safe "gdal.h GDALRasterIO" rasterIO_
  :: Band a -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> CInt -> CInt
  -> CInt -> CInt -> CInt -> IO CInt


data Block where
    Block :: (Typeable a, Storable a) => Vector a -> Block

type MaybeIOVector a = IO (Maybe (Vector a))

readBandBlock :: (Storable a, Typeable a)
  => Band b -> Int -> Int -> MaybeIOVector a
readBandBlock b x y =
  liftM (maybe Nothing (\(Block a) -> cast a)) $ readBandBlock' b x y


readBandBlock' :: Band b -> Int -> Int -> IO (Maybe Block)
readBandBlock' b x y = 
  case bandDatatype b of
    GDT_Byte     -> Just . Block <$> (readIt b x y :: IOVector Word8)
    GDT_Int16    -> Just . Block <$> (readIt b x y :: IOVector Int16)
    GDT_Int32    -> Just . Block <$> (readIt b x y :: IOVector Int32)
    GDT_Float32  -> Just . Block <$> (readIt b x y :: IOVector Float)
    GDT_Float64  -> Just . Block <$> (readIt b x y :: IOVector Double)
    GDT_CInt16   -> Just . Block <$> (readIt b x y :: IOVector (Complex Float))
    GDT_CInt32   -> Just . Block <$> (readIt b x y :: IOVector (Complex Double))
    GDT_CFloat32 -> Just . Block <$> (readIt b x y :: IOVector (Complex Float))
    GDT_CFloat64 -> Just . Block <$> (readIt b x y :: IOVector (Complex Double))
    _            -> return Nothing
  where
    readIt :: Storable a => Band b -> Int -> Int -> IOVector a
    readIt b x y = do
      f <- mallocForeignPtrArray (bandBlockLen b)
      withForeignPtr f $ \ptr ->
        throwIfError "could not read block" $
          readBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)
      return $ unsafeFromForeignPtr0 f (bandBlockLen b)

foreign import ccall safe "gdal.h GDALReadBlock" readBlock_
    :: Band a -> CInt -> CInt -> Ptr () -> IO CInt


type IOVector a = IO (Vector a)

writeBandBlock :: (Storable a, Typeable a)
  => RWBand
  -> Int -> Int
  -> Vector a
  -> IO ()
writeBandBlock b x y vec = do
    let nElems    = bandBlockLen b
        (fp, len) = unsafeToForeignPtr0 vec
    if nElems /= len 
      then throw $ GDALException CE_Failure "wrongly sized vector"
      else if typeOf vec /= typeOfBand b
           then throw $ GDALException CE_Failure "wrongly typed vector"
           else withForeignPtr fp $ \ptr ->
                throwIfError "could not write block" $
                   writeBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)

foreign import ccall safe "gdal.h GDALWriteBlock" writeBlock_
   :: RWBand -> CInt -> CInt -> Ptr () -> IO CInt


typeOfBand = typeOfdatatype . bandDatatype

typeOfdatatype dt =
  case dt of
    GDT_Byte     -> typeOf (undefined :: Vector Word8)
    GDT_Int16    -> typeOf (undefined :: Vector Int16)
    GDT_Int32    -> typeOf (undefined :: Vector Int32)
    GDT_Float32  -> typeOf (undefined :: Vector Float)
    GDT_Float64  -> typeOf (undefined :: Vector Double)
    GDT_CInt16   -> typeOf (undefined :: Vector (Complex Float))
    GDT_CInt32   -> typeOf (undefined :: Vector (Complex Double))
    GDT_CFloat32 -> typeOf (undefined :: Vector (Complex Float))
    GDT_CFloat64 -> typeOf (undefined :: Vector (Complex Double))
    _            -> typeOf (undefined :: Bool) -- will never match a vector

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral


toOptionList :: [(String,String)] -> IO (Ptr CString)
toOptionList opts =  foldM folder nullPtr opts
  where folder acc (k,v) = withCString k $ \k' -> withCString v $ \v' ->
                           {#call unsafe CSLSetNameValue as ^#} acc k' v'

type Mutex = MVar ()

newMutex :: IO Mutex
newMutex = newMVar ()


acquireMutex :: Mutex -> IO ()
acquireMutex = takeMVar

releaseMutex :: Mutex -> IO ()
releaseMutex m = putMVar m ()

withMutex m action = finally (acquireMutex m >> action) (releaseMutex m)
