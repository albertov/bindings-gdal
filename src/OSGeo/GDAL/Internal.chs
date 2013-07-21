{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OSGeo.GDAL.Internal (
    HasDataset
  , HasBand
  , HasWritebaleBand
  , HasDatatype
  , Datatype (..)
  , GDALException
  , isGDALException
  , Geotransform (..)
  , IOVector
  , DriverOptions
  , MajorObject
  , Dataset
  , ReadWrite
  , ReadOnly
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , Driver
  , ColorTable
  , RasterAttributeTable
  , GComplex (..)

  , setQuietErrorHandler
  , unDataset
  , unBand

  , withAllDriversRegistered
  , registerAllDrivers
  , destroyDriverManager
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
import Control.Exception (bracket, throw, Exception(..), SomeException,
                          finally)
import Control.Monad (liftM, foldM)

import Data.Int (Int16, Int32)
import qualified Data.Complex as Complex
import Data.Maybe (isJust)
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

import OSGeo.Util

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

{#pointer GDALMajorObjectH as MajorObject newtype#}
{#pointer GDALDatasetH as Dataset foreign newtype nocode#}

data ReadOnly
data ReadWrite
newtype (Dataset a t) = Dataset (ForeignPtr (Dataset a t), Mutex)

unDataset (Dataset (d, _)) = d

type RODataset = Dataset ReadOnly
type RWDataset = Dataset ReadWrite
withDataset, withDataset' :: (Dataset a t) -> (Ptr (Dataset a t) -> IO b) -> IO b
withDataset ds@(Dataset (_, m)) fun = withMutex m $ withDataset' ds fun

withDataset' (Dataset (fptr,_)) = withForeignPtr fptr

{#pointer GDALRasterBandH as Band newtype nocode#}
newtype (Band a t) = Band (Ptr ((Band a t)))

unBand (Band b) = b

type ROBand = Band ReadOnly
type RWBand = Band ReadWrite


{#pointer GDALDriverH as Driver newtype#}
{#pointer GDALColorTableH as ColorTable newtype#}
{#pointer GDALRasterAttributeTableH as RasterAttributeTable newtype#}

{#fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{#fun GDALDestroyDriverManager as destroyDriverManager {} -> `()'#}

withAllDriversRegistered act
  = registerAllDrivers >> finally act destroyDriverManager

{# fun unsafe GDALGetDriverByName as c_driverByName
    { `String' } -> `Driver' id #}

driverByName :: String -> IO Driver
driverByName s = do
    driver@(Driver ptr) <- c_driverByName s
    if ptr==nullPtr
        then throw $ GDALException CE_Failure "failed to load driver"
        else return driver

type DriverOptions = [(String,String)]

class ( HasDatatype t
      , a ~ ReadWrite
      , d ~ Dataset,
      HasWritebaleBand Band ReadWrite t)
  => HasDataset d a t where
    create :: String -> String -> Int -> Int -> Int -> DriverOptions
           -> IO (d a t)
    create drv path nx ny bands options = do
        d <- driverByName  drv
        create' d path nx ny bands (datatype (undefined :: t)) options

create' :: Driver -> String -> Int -> Int -> Int -> Datatype -> DriverOptions
       -> IO (Dataset ReadWrite t)
create' drv path nx ny bands dtype options = withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC dtype
    withOptionList options $ \opts ->
        create_ drv path' nx' ny' bands' dtype' opts >>= newDatasetHandle

withOptionList ::
  [(String, String)] -> (Ptr CString -> IO c) -> IO c
withOptionList opts = bracket (toOptionList opts) freeOptionList
  where toOptionList = foldM folder nullPtr
        folder acc (k,v) = withCString k $ \k' -> withCString v $ \v' ->
                           {#call unsafe CSLSetNameValue as ^#} acc k' v'
        freeOptionList = {#call unsafe CSLDestroy as ^#} . castPtr

instance HasDataset Dataset ReadWrite Word8 where
instance HasDataset Dataset ReadWrite Int16 where
instance HasDataset Dataset ReadWrite Int32 where
instance HasDataset Dataset ReadWrite Word16 where
instance HasDataset Dataset ReadWrite Word32 where
instance HasDataset Dataset ReadWrite Float where
instance HasDataset Dataset ReadWrite Double where
instance HasDataset Dataset ReadWrite (GComplex Int16) where
instance HasDataset Dataset ReadWrite (GComplex Int32) where
instance HasDataset Dataset ReadWrite (GComplex Float) where
instance HasDataset Dataset ReadWrite (GComplex Double) where


foreign import ccall safe "gdal.h GDALCreate" create_
  :: Driver
  -> CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CString
  -> IO (Ptr (RWDataset t))

openReadOnly :: String -> IO (RODataset t)
openReadOnly p = withCString p openIt
  where openIt p' = open_ p' (fromEnumC GA_ReadOnly) >>= newDatasetHandle

openReadWrite :: String -> IO (RWDataset t)
openReadWrite p = withCString p openIt
  where openIt p' = open_ p' (fromEnumC GA_Update) >>= newDatasetHandle

foreign import ccall safe "gdal.h GDALOpen" open_
   :: CString -> CInt -> IO (Ptr (Dataset a t))


createCopy' ::
  Driver -> String -> (Dataset a t) -> Bool -> DriverOptions
  -> ProgressFun -> IO (RWDataset t)
createCopy' driver path dataset strict options progressFun
  = withCString path $ \p ->
    withDataset dataset $ \ds ->
    withProgressFun progressFun $ \pFunc -> do
        let s = fromBool strict
        withOptionList options $ \o ->
            createCopy_ driver p ds s o pFunc (castPtr nullPtr)
            >>= newDatasetHandle


createCopy ::
  String -> String -> (Dataset a t) -> Bool -> DriverOptions
  -> IO (RWDataset t)
createCopy driver path dataset strict options = do
  d <- driverByName driver
  createCopy' d path dataset strict options (\_ _ _ -> return 1)

foreign import ccall safe "gdal.h GDALCreateCopy" createCopy_
  :: Driver
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
        then throw $ GDALException CE_Failure "Could not create dataset"
        else do fp <- newForeignPtr closeDataset p
                mutex <- newMutex
                return $ Dataset (fp, mutex)

foreign import ccall "gdal.h &GDALClose"
  closeDataset :: FunPtr (Ptr (Dataset a t) -> IO ())

createMem:: HasDataset d a t
  => Int -> Int -> Int -> DriverOptions -> IO (d a t)
createMem = create "MEM" ""

flushCache :: forall a t. Dataset a t -> IO ()
flushCache d = withDataset d flushCache'

foreign import ccall safe "gdal.h GDALFlushCache" flushCache'
  :: Ptr (Dataset a t) -> IO ()


datasetProjection :: Dataset a t -> IO String
datasetProjection d = withDataset d $ \d' -> do
    p <- getProjection_ d'
    peekCString p

foreign import ccall unsafe "gdal.h GDALGetProjectionRef" getProjection_
  :: Ptr (Dataset a t) -> IO (Ptr CChar)


setDatasetProjection :: (RWDataset t) -> String -> IO ()
setDatasetProjection d p = throwIfError "could not set projection" f
  where f = withDataset d $ \d' -> withCString p $ \p' -> setProjection' d' p'

foreign import ccall unsafe "gdal.h GDALSetProjection" setProjection'
  :: Ptr (Dataset a t) -> Ptr CChar -> IO CInt


data Geotransform = Geotransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

datasetGeotransform :: Dataset a t -> IO Geotransform
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

datasetBandCount :: Dataset a t -> IO (Int)
datasetBandCount d = liftM fromIntegral $ withDataset d bandCount_

foreign import ccall unsafe "gdal.h GDALGetRasterCount" bandCount_
  :: Ptr (Dataset a t) -> IO CInt

withBand :: Dataset a t -> Int -> (Band a t -> IO b) -> IO b
withBand ds band f = withDataset ds $ \dPtr -> do
    rBand@(Band p) <- getRasterBand dPtr (fromIntegral band)
    if p == nullPtr
        then throw $
            GDALException CE_Failure ("could not get band #" ++ show band)
        else f rBand

foreign import ccall unsafe "gdal.h GDALGetRasterBand" getRasterBand
  :: Ptr (Dataset a t) -> CInt -> IO ((Band a t))


bandDatatype :: (Band a t) -> Datatype
bandDatatype band = toEnumC . getDatatype_ $ band

foreign import ccall unsafe "gdal.h GDALGetRasterDataType" getDatatype_
  :: (Band a t) -> CInt


bandBlockSize :: (Band a t) -> (Int,Int)
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   -- unsafePerformIO is safe here since the block size cant change once
   -- a dataset is created
   getBlockSize_ band xPtr yPtr
   liftA2 (,) (liftM fromIntegral $ peek xPtr) (liftM fromIntegral $ peek yPtr)

foreign import ccall unsafe "gdal.h GDALGetBlockSize" getBlockSize_
    :: (Band a t) -> Ptr CInt -> Ptr CInt -> IO ()


bandBlockLen :: (Band a t) -> Int
bandBlockLen = uncurry (*) . bandBlockSize

bandSize :: (Band a t) -> (Int, Int)
bandSize band
  = (fromIntegral . getXSize_ $ band, fromIntegral . getYSize_ $ band)

foreign import ccall unsafe "gdal.h GDALGetRasterBandXSize" getXSize_
  :: (Band a t) -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterBandYSize" getYSize_
  :: (Band a t) -> CInt


bandNodataValue :: (Band a t) -> IO (Maybe Double)
bandNodataValue b = alloca $ \p -> do
   value <- liftM realToFrac $ getNodata_ b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
   
foreign import ccall unsafe "gdal.h GDALGetRasterNoDataValue" getNodata_
   :: (Band a t) -> Ptr CInt -> IO CDouble
   

setBandNodataValue :: (RWBand t) -> Double -> IO ()
setBandNodataValue b v = throwIfError "could not set nodata" $
                            setNodata_ b (realToFrac v)

foreign import ccall safe "gdal.h GDALSetRasterNoDataValue" setNodata_
    :: (RWBand t) -> CDouble -> IO CInt


fillBand :: (RWBand t) -> Double -> Double -> IO ()
fillBand b r i = throwIfError "could not fill band" $
    fillRaster_ b (realToFrac r) (realToFrac i)

foreign import ccall safe "gdal.h GDALFillRaster" fillRaster_
    :: (RWBand t) -> CDouble -> CDouble -> IO CInt




data GComplex a = (:+) !a !a deriving (Eq, Show, Typeable)
infix 6 :+

class IsGComplex a where
   type ComplexType a :: *
   toComplex :: GComplex a -> Complex.Complex (ComplexType a)
   fromComplex :: Complex.Complex (ComplexType a) -> GComplex a

   toComplex (r :+  i) = (toUnit r) Complex.:+ (toUnit i)
   fromComplex (r Complex.:+ i) = (fromUnit r) :+ (fromUnit i)

   toUnit :: a -> ComplexType a
   fromUnit :: ComplexType a -> a

instance IsGComplex Int16 where
   type ComplexType Int16 = Float
   toUnit = fromIntegral
   fromUnit = round

instance IsGComplex Int32 where
   type ComplexType Int32 = Double
   toUnit = fromIntegral
   fromUnit = round

instance IsGComplex Float where
   type ComplexType Float = Float
   toUnit = id
   fromUnit = id

instance IsGComplex Double where
   type ComplexType Double = Double
   toUnit = id
   fromUnit = id


instance Storable a => Storable (GComplex a) where
  sizeOf _ = sizeOf (undefined :: a) * 2
  alignment _ = alignment (undefined :: a)
 
  {-# SPECIALIZE INLINE peek ::
      Ptr (GComplex Double) -> IO (GComplex Double) #-}
  {-# SPECIALIZE INLINE peek ::
      Ptr (GComplex Float) -> IO (GComplex Float) #-}
  {-# SPECIALIZE INLINE peek ::
      Ptr (GComplex Int16) -> IO (GComplex Int16) #-}
  {-# SPECIALIZE INLINE peek ::
      Ptr (GComplex Int32) -> IO (GComplex Int32) #-}
  peek p = (:+) <$> peekElemOff (castPtr p) 0 <*> peekElemOff (castPtr p) 1

  {-# SPECIALIZE INLINE
      poke :: Ptr (GComplex Float) -> GComplex Float -> IO () #-}
  {-# SPECIALIZE INLINE
      poke :: Ptr (GComplex Double) -> GComplex Double -> IO () #-}
  {-# SPECIALIZE INLINE
      poke :: Ptr (GComplex Int16) -> GComplex Int16 -> IO () #-}
  {-# SPECIALIZE INLINE
      poke :: Ptr (GComplex Int32) -> GComplex Int32 -> IO () #-}
  poke p (r :+ i)
    = pokeElemOff (castPtr p) 0 r >> pokeElemOff (castPtr p) 1 i



readBand :: forall a b t. HasDatatype a
  => (Band b t)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> IO (Vector a)
readBand band xoff yoff sx sy bx by pxs lns = do
    fp <- mallocForeignPtrArray (bx * by)
    let dtype = fromEnumC (datatype (undefined :: a))
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
    :: (Band a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
    -> Ptr (Ptr CChar) -> IO CInt

        
writeBand :: forall a t. HasDatatype a
  => (RWBand t)
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
            (fromEnumC (datatype (undefined :: a)))
            (fromIntegral pxs)
            (fromIntegral lns)

foreign import ccall safe "gdal.h GDALRasterIO" rasterIO_
  :: (Band a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> CInt -> CInt
  -> CInt -> CInt -> CInt -> IO CInt


type IOVector a = IO (Vector a)

class (HasDatatype t, b ~ Band)
  => HasBand b a t where
    readBandBlock  :: b a t -> Int -> Int -> IOVector t
    readBandBlock b x y
      = if not $ isValidDatatype b (undefined :: Vector t)
            then throw $ GDALException CE_Failure "wrong band type"
            else do
                f <- mallocForeignPtrArray (bandBlockLen b)
                withForeignPtr f $ \ptr ->
                  throwIfError "could not read block" $
                    readBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)
                return $ unsafeFromForeignPtr0 f (bandBlockLen b)

foreign import ccall safe "gdal.h GDALReadBlock" readBlock_
    :: (Band a t) -> CInt -> CInt -> Ptr () -> IO CInt

class (HasBand b a t, a ~ ReadWrite) => HasWritebaleBand b a t where
    writeBandBlock :: b a t -> Int -> Int -> Vector t -> IO ()
    writeBandBlock b x y vec = do
        let nElems    = bandBlockLen b
            (fp, len) = unsafeToForeignPtr0 vec
        if nElems /= len
           then throw $ GDALException CE_Failure "wrongly sized vector"
           else if not $ isValidDatatype b vec
                then throw $ GDALException CE_Failure "wrongly typed vector"
                else withForeignPtr fp $ \ptr ->
                     throwIfError "could not write block" $
                        writeBlock_ b (fromIntegral x) (fromIntegral y)
                                      (castPtr ptr)

foreign import ccall safe "gdal.h GDALWriteBlock" writeBlock_
   :: (RWBand t) -> CInt -> CInt -> Ptr () -> IO CInt



instance forall a. HasBand Band a Word8 where
instance forall a. HasBand Band a Word16 where
instance forall a. HasBand Band a Word32 where
instance forall a. HasBand Band a Int16 where
instance forall a. HasBand Band a Int32 where
instance forall a. HasBand Band a Float where
instance forall a. HasBand Band a Double where
instance forall a. HasBand Band a (GComplex Int16) where
instance forall a. HasBand Band a (GComplex Int32) where
instance forall a. HasBand Band a (GComplex Float) where
instance forall a. HasBand Band a (GComplex Double) where

instance HasWritebaleBand Band ReadWrite Word8
instance HasWritebaleBand Band ReadWrite Word16
instance HasWritebaleBand Band ReadWrite Word32
instance HasWritebaleBand Band ReadWrite Int16
instance HasWritebaleBand Band ReadWrite Int32
instance HasWritebaleBand Band ReadWrite Float
instance HasWritebaleBand Band ReadWrite Double
instance HasWritebaleBand Band ReadWrite (GComplex Int16) where
instance HasWritebaleBand Band ReadWrite (GComplex Int32) where
instance HasWritebaleBand Band ReadWrite (GComplex Float) where
instance HasWritebaleBand Band ReadWrite (GComplex Double) where

class (Storable a, Typeable a) => HasDatatype a where
    datatype :: a -> Datatype

instance HasDatatype Word8  where datatype _ = GDT_Byte
instance HasDatatype Word16 where datatype _ = GDT_UInt16
instance HasDatatype Word32 where datatype _ = GDT_UInt32
instance HasDatatype Int16  where datatype _ = GDT_Int16
instance HasDatatype Int32  where datatype _ = GDT_Int32
instance HasDatatype Float  where datatype _ = GDT_Float32
instance HasDatatype Double where datatype _ = GDT_Float64
instance HasDatatype (GComplex Int16) where datatype _ = GDT_CInt16
instance HasDatatype (GComplex Int32) where datatype _ = GDT_CInt32
instance HasDatatype (GComplex Float) where datatype _ = GDT_CFloat32
instance HasDatatype (GComplex Double) where datatype _ = GDT_CFloat64

isValidDatatype :: forall a t v.  Typeable v
  => Band a t -> v -> Bool
isValidDatatype b v = typeOfBand b == typeOf v
  where
    typeOfBand        = typeOfdatatype . bandDatatype
    typeOfdatatype dt =
      case dt of
        GDT_Byte     -> typeOf (undefined :: Vector Word8)
        GDT_UInt16   -> typeOf (undefined :: Vector Word16)
        GDT_UInt32   -> typeOf (undefined :: Vector Word32)
        GDT_Int16    -> typeOf (undefined :: Vector Int16)
        GDT_Int32    -> typeOf (undefined :: Vector Int32)
        GDT_Float32  -> typeOf (undefined :: Vector Float)
        GDT_Float64  -> typeOf (undefined :: Vector Double)
        GDT_CInt16   -> typeOf (undefined :: Vector (GComplex Int16))
        GDT_CInt32   -> typeOf (undefined :: Vector (GComplex Int32))
        GDT_CFloat32 -> typeOf (undefined :: Vector (GComplex Float))
        GDT_CFloat64 -> typeOf (undefined :: Vector (GComplex Double))
        _            -> typeOf (undefined :: Bool) -- will never match a vector

