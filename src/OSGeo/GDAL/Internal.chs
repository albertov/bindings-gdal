{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module OSGeo.GDAL.Internal (
    GDALType
  , Datatype (..)
  , GDALException (..)
  , isGDALException
  , Geotransform (..)
  , DriverOptions
  , Driver (..)
  , Dataset
  , ReadWrite
  , ReadOnly
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band

  , setQuietErrorHandler
  , unDataset
  , unBand

  , registerAllDrivers
  , destroyDriverManager
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

  , datasetSize
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

  -- Internal Util
  , throwIfError
  , withDataset
) where

import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Exception (bracket, throw, Exception(..), SomeException)
import Control.Monad (liftM, foldM)

import Data.Int (Int16, Int32)
import Data.Complex (Complex(..))
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeOf)
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
import System.Process (readProcess)
import Data.Char (toUpper)

import OSGeo.Util

#include "gdal.h"
#include "cpl_string.h"
#include "cpl_error.h"


$(let names = fmap (words . map toUpper) $
                readProcess "gdal-config" ["--formats"] ""
  in createEnum "Driver" names)

data GDALException = Unknown !Error !String
                   | InvalidType
                   | InvalidRasterSize !Int !Int
                   | InvalidBlockSize
                   | DriverLoadError
                   | NullDatasetHandle
                   | InvalidBand !Int
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
      e'      -> throw $ Unknown e' msg

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

{#pointer GDALDatasetH as Dataset foreign newtype nocode#}

data ReadOnly
data ReadWrite
newtype Dataset a t = Dataset (ForeignPtr (Dataset a t), Mutex)

unDataset :: Dataset a t -> ForeignPtr (Dataset a t)
unDataset (Dataset (d, _)) = d

type RODataset = Dataset ReadOnly
type RWDataset = Dataset ReadWrite

withDataset :: (Dataset a t) -> (Ptr (Dataset a t) -> IO b) -> IO b
withDataset (Dataset (fptr,_)) = withForeignPtr fptr

{#pointer GDALRasterBandH as Band newtype nocode#}
newtype (Band s a t) = Band (Ptr (Band s a t))

unBand :: Band s a t -> Ptr (Band s a t)
unBand (Band b) = b

type ROBand s = Band s ReadOnly
type RWBand s = Band s ReadWrite


{#pointer GDALDriverH as DriverH newtype#}

{#fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{#fun GDALDestroyDriverManager as destroyDriverManager {} -> `()'#}

{# fun unsafe GDALGetDriverByName as c_driverByName
    { `String' } -> `DriverH' id #}

driverByName :: Driver -> IO DriverH
driverByName s = do
    driver@(DriverH ptr) <- c_driverByName (show s)
    if ptr==nullPtr
        then throw DriverLoadError
        else return driver

type DriverOptions = [(String,String)]

create
  :: forall t. GDALType t
  => Driver -> FilePath -> Int -> Int -> Int -> DriverOptions
  -> IO (Dataset ReadWrite t)
create drv path nx ny bands options = do
    create' drv path nx ny bands (datatype (Proxy :: Proxy t)) options

create'
  :: forall t. GDALType t
  => Driver -> String -> Int -> Int -> Int -> Datatype -> DriverOptions
  -> IO (Dataset ReadWrite t)
create' drv path nx ny bands dtype options = withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC dtype
    d <- driverByName drv
    withOptionList options $ \opts ->
        c_create d path' nx' ny' bands' dtype' opts >>= newDatasetHandle

withOptionList ::
  [(String, String)] -> (Ptr CString -> IO c) -> IO c
withOptionList opts = bracket (toOptionList opts) freeOptionList
  where toOptionList = foldM folder nullPtr
        folder acc (k,v) = withCString k $ \k' -> withCString v $ \v' ->
                           {#call unsafe CSLSetNameValue as ^#} acc k' v'
        freeOptionList = {#call unsafe CSLDestroy as ^#} . castPtr


foreign import ccall safe "gdal.h GDALCreate" c_create
  :: DriverH
  -> CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CString
  -> IO (Ptr (RWDataset t))

openReadOnly :: GDALType a => FilePath -> IO (RODataset a)
openReadOnly p = withCString p $ \p' ->
  open_ p' (fromEnumC GA_ReadOnly) >>= newDatasetHandle >>= checkType

openReadWrite :: GDALType a => FilePath -> IO (RWDataset a)
openReadWrite p = withCString p $ \p' ->
  open_ p' (fromEnumC GA_Update) >>= newDatasetHandle >>= checkType

checkType :: forall t a. GDALType a => Dataset t a -> IO (Dataset t a)
checkType ds
  | datasetBandCount ds > 0 = withBand ds 1 $ \b ->
                                if isValidDatatype b (Proxy :: Proxy a)
                                  then return ds
                                  else throw InvalidType
  | otherwise = return ds

foreign import ccall safe "gdal.h GDALOpen" open_
   :: CString -> CInt -> IO (Ptr (Dataset a t))


createCopy' ::
  Driver -> String -> (Dataset a t) -> Bool -> DriverOptions
  -> ProgressFun -> IO (RWDataset t)
createCopy' driver path dataset strict options progressFun = do
  d <- driverByName driver
  withCString path $ \p ->
    withDataset dataset $ \ds ->
    withProgressFun progressFun $ \pFunc -> do
        let s = fromBool strict
        withOptionList options $ \o ->
            c_createCopy d p ds s o pFunc (castPtr nullPtr)
            >>= newDatasetHandle


createCopy ::
  Driver -> FilePath -> (Dataset a t) -> Bool -> DriverOptions
  -> IO (RWDataset t)
createCopy driver path dataset strict options = do
  createCopy' driver path dataset strict options (\_ _ _ -> return 1)

foreign import ccall safe "gdal.h GDALCreateCopy" c_createCopy
  :: DriverH
  -> Ptr CChar
  -> Ptr (Dataset a t)
  -> CInt
  -> Ptr CString
  -> FunPtr ProgressFun
  -> Ptr ()
  -> IO (Ptr (RWDataset t))


withProgressFun :: forall c.
  ProgressFun -> (FunPtr ProgressFun -> IO c) -> IO c
withProgressFun = withCCallback wrapProgressFun

withCCallback :: forall c t a.
  (t -> IO (FunPtr a)) -> t -> (FunPtr a -> IO c) -> IO c
withCCallback w f = bracket (w f) freeHaskellFunPtr

type ProgressFun = CDouble -> Ptr CChar -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  wrapProgressFun :: ProgressFun -> IO (FunPtr ProgressFun)


newDatasetHandle :: Ptr (Dataset a t) -> IO (Dataset a t)
newDatasetHandle p =
    if p==nullPtr
        then throw NullDatasetHandle
        else do fp <- newForeignPtr closeDataset p
                mutex <- newMutex
                return $ Dataset (fp, mutex)

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset a t) -> IO ())

createMem
  :: GDALType t
  => Int -> Int -> Int -> DriverOptions -> IO (Dataset ReadWrite t)
createMem = create MEM ""

flushCache :: forall t. RWDataset t -> IO ()
flushCache d = withDataset d flushCache'

foreign import ccall safe "gdal.h GDALFlushCache" flushCache'
  :: Ptr (Dataset a t) -> IO ()

datasetSize :: Dataset a t -> (Int, Int)
datasetSize ds = unsafePerformIO $ withDataset ds $ \dsPtr ->
    return ( fromIntegral . getDatasetXSize_ $ dsPtr
           , fromIntegral . getDatasetYSize_ $ dsPtr)

foreign import ccall unsafe "gdal.h GDALGetRasterXSize" getDatasetXSize_
  :: Ptr (Dataset a t) -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterYSize" getDatasetYSize_
  :: Ptr (Dataset a t) -> CInt

datasetProjection :: Dataset a t -> IO String
datasetProjection d = withDataset d $ \d' -> do
    p <- getProjection_ d'
    peekCString p

foreign import ccall unsafe "gdal.h GDALGetProjectionRef" getProjection_
  :: Ptr (Dataset a t) -> IO (Ptr CChar)


setDatasetProjection :: (RWDataset t) -> String -> IO ()
setDatasetProjection d p = throwIfError
                           "setDatasetProjection: could not set projection" f
  where f = withDataset d $ \d' -> withCString p $ \p' -> setProjection' d' p'

foreign import ccall unsafe "gdal.h GDALSetProjection" setProjection'
  :: Ptr (Dataset a t) -> Ptr CChar -> IO CInt


data Geotransform = Geotransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

datasetGeotransform :: Dataset a t -> IO Geotransform
datasetGeotransform ds = withDataset ds $ \dPtr -> do
    allocaArray 6 $ \a -> do
      throwIfError "could not get geotransform" $ getGeoTransform dPtr a
      Geotransform <$> liftM realToFrac (peekElemOff a 0)
                   <*> liftM realToFrac (peekElemOff a 1)
                   <*> liftM realToFrac (peekElemOff a 2)
                   <*> liftM realToFrac (peekElemOff a 3)
                   <*> liftM realToFrac (peekElemOff a 4)
                   <*> liftM realToFrac (peekElemOff a 5)

foreign import ccall unsafe "gdal.h GDALGetGeoTransform" getGeoTransform
  :: Ptr (Dataset a t) -> Ptr CDouble -> IO CInt

setDatasetGeotransform :: (RWDataset t) -> Geotransform -> IO ()
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
  :: Ptr (Dataset a t) -> Ptr CDouble -> IO CInt

datasetBandCount :: Dataset a t -> Int
datasetBandCount d
  = unsafePerformIO $ liftM fromIntegral $ withDataset d bandCount_

foreign import ccall unsafe "gdal.h GDALGetRasterCount" bandCount_
  :: Ptr (Dataset a t) -> IO CInt

withBand :: Dataset a t -> Int -> (forall s. Band s a t -> IO b) -> IO b
withBand ds band f = withDataset ds $ \dPtr -> do
    rBand@(Band p) <- getRasterBand dPtr (fromIntegral band)
    if p == nullPtr
        then throw (InvalidBand band)
        else f rBand

foreign import ccall unsafe "gdal.h GDALGetRasterBand" getRasterBand
  :: Ptr (Dataset a t) -> CInt -> IO ((Band s a t))


bandDatatype :: (Band s a t) -> Datatype
bandDatatype band = toEnumC . getDatatype_ $ band

foreign import ccall unsafe "gdal.h GDALGetRasterDataType" getDatatype_
  :: (Band s a t) -> CInt


bandBlockSize :: (Band s a t) -> (Int,Int)
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   -- unsafePerformIO is safe here since the block size cant change once
   -- a dataset is created
   getBlockSize_ band xPtr yPtr
   liftA2 (,) (liftM fromIntegral $ peek xPtr) (liftM fromIntegral $ peek yPtr)

foreign import ccall unsafe "gdal.h GDALGetBlockSize" getBlockSize_
    :: (Band s a t) -> Ptr CInt -> Ptr CInt -> IO ()


bandBlockLen :: (Band s a t) -> Int
bandBlockLen = uncurry (*) . bandBlockSize

bandSize :: (Band s a t) -> (Int, Int)
bandSize band
  = (fromIntegral . getBandXSize_ $ band, fromIntegral . getBandYSize_ $ band)

foreign import ccall unsafe "gdal.h GDALGetRasterBandXSize" getBandXSize_
  :: (Band s a t) -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterBandYSize" getBandYSize_
  :: (Band s a t) -> CInt


bandNodataValue :: (Band s a t) -> IO (Maybe Double)
bandNodataValue b = alloca $ \p -> do
   value <- liftM realToFrac $ getNodata_ b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
   
foreign import ccall unsafe "gdal.h GDALGetRasterNoDataValue" getNodata_
   :: (Band s a t) -> Ptr CInt -> IO CDouble
   

setBandNodataValue :: (RWBand s t) -> Double -> IO ()
setBandNodataValue b v = throwIfError "could not set nodata" $
                            setNodata_ b (realToFrac v)

foreign import ccall safe "gdal.h GDALSetRasterNoDataValue" setNodata_
    :: (RWBand s t) -> CDouble -> IO CInt


fillBand :: (RWBand s t) -> Double -> Double -> IO ()
fillBand b r i = throwIfError "could not fill band" $
    fillRaster_ b (realToFrac r) (realToFrac i)

foreign import ccall safe "gdal.h GDALFillRaster" fillRaster_
    :: (RWBand s t) -> CDouble -> CDouble -> IO CInt


readBand :: forall s t a. GDALType a
  => (Band s t a)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> IO (Vector a)
readBand band xoff yoff sx sy bx by pxs lns = do
    fp <- mallocForeignPtrArray (bx * by)
    let dtype = fromEnumC (datatype (Proxy :: Proxy a))
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
          dtype
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
          dtype
          (fromIntegral pxs)
          (fromIntegral lns)
    return $ unsafeFromForeignPtr0 fp (bx * by)

foreign import ccall safe "gdal.h GDALRasterAdviseRead" adviseRead_
    :: (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
    -> Ptr (Ptr CChar) -> IO CInt

        
writeBand :: forall s a. GDALType a
  => (RWBand s a)
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
      then throw $ InvalidRasterSize bx by
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
            (fromEnumC (datatype (Proxy :: Proxy a)))
            (fromIntegral pxs)
            (fromIntegral lns)

foreign import ccall safe "gdal.h GDALRasterIO" rasterIO_
  :: (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> CInt -> CInt
  -> CInt -> CInt -> CInt -> IO CInt

readBandBlock
  :: forall s t a. GDALType a
  => Band s t a -> Int -> Int -> IO (Vector a)
readBandBlock b x y = do
  f <- mallocForeignPtrArray (bandBlockLen b)
  withForeignPtr f $ \ptr ->
    throwIfError "could not read block" $
      readBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)
  return $ unsafeFromForeignPtr0 f (bandBlockLen b)

foreign import ccall safe "gdal.h GDALReadBlock" readBlock_
    :: (Band s a t) -> CInt -> CInt -> Ptr () -> IO CInt

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> Int -> Int -> Vector a -> IO ()
writeBandBlock b x y vec = do
  let nElems    = bandBlockLen b
      (fp, len) = unsafeToForeignPtr0 vec
  if nElems /= len
    then throw InvalidBlockSize
    else withForeignPtr fp $ \ptr ->
         throwIfError "could not write block" $
            writeBlock_ b (fromIntegral x) (fromIntegral y)
                          (castPtr ptr)

foreign import ccall safe "gdal.h GDALWriteBlock" writeBlock_
   :: (RWBand s a) -> CInt -> CInt -> Ptr () -> IO CInt



class (Storable a, Typeable a) => GDALType a where
    datatype :: Proxy a -> Datatype

instance GDALType Word8  where datatype _ = GDT_Byte
instance GDALType Word16 where datatype _ = GDT_UInt16
instance GDALType Word32 where datatype _ = GDT_UInt32
instance GDALType Int16  where datatype _ = GDT_Int16
instance GDALType Int32  where datatype _ = GDT_Int32
instance GDALType Float  where datatype _ = GDT_Float32
instance GDALType Double where datatype _ = GDT_Float64
instance GDALType (Complex Int16) where datatype _ = GDT_CInt16
instance GDALType (Complex Int32) where datatype _ = GDT_CInt32
instance GDALType (Complex Float) where datatype _ = GDT_CFloat32
instance GDALType (Complex Double) where datatype _ = GDT_CFloat64

isValidDatatype :: forall s a t v.  Typeable v
  => Band s a t -> Proxy v -> Bool
isValidDatatype b v
  = let vt = typeOf v
    in case bandDatatype b of
      GDT_Byte     -> vt == typeOf (Proxy :: Proxy Word8)
      GDT_UInt16   -> vt == typeOf (Proxy :: Proxy Word16)
      GDT_UInt32   -> vt == typeOf (Proxy :: Proxy Word32)
      GDT_Int16    -> vt == typeOf (Proxy :: Proxy Int16)
      GDT_Int32    -> vt == typeOf (Proxy :: Proxy Int32)
      GDT_Float32  -> vt == typeOf (Proxy :: Proxy Float)
      GDT_Float64  -> vt == typeOf (Proxy :: Proxy Double)
      GDT_CInt16   -> vt == typeOf (Proxy :: Proxy (Complex Int16))
      GDT_CInt32   -> vt == typeOf (Proxy :: Proxy (Complex Int32))
      GDT_CFloat32 -> vt == typeOf (Proxy :: Proxy (Complex Float))
      GDT_CFloat64 -> vt == typeOf (Proxy :: Proxy (Complex Double))
      _            -> False
