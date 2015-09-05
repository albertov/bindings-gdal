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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OSGeo.GDAL.Internal (
    GDAL
  , runGDAL
  , GDALType
  , Datatype (..)
  , GDALException (..)
  , isGDALException
  , Geotransform (..)
  , DriverOptions
  , Driver (..)
  , Dataset
  , SomeDataset (..)
  , ReadWrite
  , ReadOnly
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , Value (..)
  , unValue
  , fromValue
  , isNoData

  , setQuietErrorHandler

  , registerAllDrivers
  , destroyDriverManager
  , create
  , createMem
  , flushCache
  , openReadOnly
  , openReadWrite
  , openAnyReadOnly
  , openAnyReadWrite
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

  , bandDatatype
  , bandBlockSize
  , bandBlockCount
  , bandSize
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , readBand
  , unsafeLazyReadBand
  , readBandBlock
  , writeBand
  , writeBandBlock
  , fillBand

  -- Internal Util
  , throwIfError
  , unsafeWithDataset
  , unsafeWithBand
) where

import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Exception ( bracket, throw, Exception(..), SomeException
                         , evaluate)
import Control.DeepSeq (NFData, force)
import Control.Monad (liftM, foldM)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, register)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Int (Int16, Int32)
import Data.Bits ((.&.))
import Data.Complex (Complex(..), realPart)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as St
import Data.Coerce (coerce)
import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, freeHaskellFunPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (toBool, fromBool)

import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import System.Process (readProcess)
import Data.Char (toUpper)

import OSGeo.Util


#include "gdal.h"
#include "cpl_string.h"
#include "cpl_error.h"


$(let names = fmap (words . map toUpper) $
                readProcess "gdal-config" ["--formats"] ""
  in createEnum "Driver" names)


class (Storable a) => GDALType a where
  datatype :: Proxy a -> Datatype
  -- | default nodata value when writing to bands with no datavalue set
  nodata   :: a
  -- | how to convert to double for use in setBandNodataValue
  toNodata :: a -> CDouble
  -- | how to convert from double for use with bandNodataValue
  fromNodata :: CDouble -> a

{# enum GDALDataType as Datatype {upcaseFirstLetter} deriving (Eq) #}

data Value a = Value !a  | NoData deriving (Eq, Show)

unValue :: Value a -> a
unValue (Value a) = a
unValue NoData = error "called unValue on NoData"
{-# INLINE unValue #-}

instance  Functor Value  where
    fmap _ NoData       = NoData
    fmap f (Value a)    = Value (f a)

instance Applicative Value where
    pure = Value

    Value f <*> m       = fmap f m
    NoData  <*> _m      = NoData

    Value _m1 *> m2     = m2
    NoData    *> _m2    = NoData

instance Monad Value  where
    (Value x) >>= k     = k x
    NoData    >>= _     = NoData

    (>>) = (*>)

    return              = Value
    fail _              = NoData

instance Storable a => Storable (Value a) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Word8)
  alignment _ = alignment (undefined :: a)

  {-# INLINE peek #-}
  peek p = do
            let p1 = (castPtr p::Ptr Word8) `plusPtr` 1
            t <- peek (castPtr p::Ptr Word8)
            if t/=0
              then fmap Value (peekElemOff (castPtr p1 :: Ptr a) 0)
              else return NoData

  {-# INLINE poke #-}
  poke p x = case x of
    NoData -> poke (castPtr p :: Ptr Word8) 0
    Value a -> do
        poke (castPtr p :: Ptr Word8) 1
        let p1 = (castPtr p :: Ptr Word8) `plusPtr` 1
        pokeElemOff (castPtr p1) 0 a

isNoData :: Value a -> Bool
isNoData NoData = True
isNoData _      = False
{-# INLINE isNoData #-}

fromValue :: a -> Value a -> a
fromValue v NoData    = v
fromValue _ (Value v) = v
{-# INLINE fromValue #-}

newtype GDAL s a = GDAL (ResourceT IO a)

deriving instance Functor (GDAL s)
deriving instance Applicative (GDAL s)
deriving instance Monad (GDAL s)
deriving instance MonadIO (GDAL s)

runGDAL :: NFData a => (forall s. GDAL s a) -> IO a
runGDAL (GDAL a) = do
  registerAllDrivers
  runResourceT (a >>= liftIO . evaluate . force)


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


data DatasetMode = ReadOnly | ReadWrite

type ReadOnly  = 'ReadOnly
type ReadWrite = 'ReadWrite

newtype Dataset s (t::DatasetMode) a = Dataset (Ptr (Dataset s t a))

unsafeWithDataset
  :: Dataset s t a -> (Ptr (Dataset s t a) -> GDAL s b) -> GDAL s b
unsafeWithDataset (Dataset p) f = f p

unsafeWithDatasetIO
  :: Dataset s t a -> (Ptr (Dataset s t a) -> IO b) -> GDAL s b
unsafeWithDatasetIO ds f = unsafeWithDataset ds (liftIO . f)

type RODataset s = Dataset s ReadOnly
type RWDataset s = Dataset s ReadWrite

{#pointer GDALRasterBandH as Band newtype nocode#}
newtype (Band s (t::DatasetMode) a) = Band (Ptr (Band s t a))

unsafeWithBand
  :: Band s t a -> (Ptr (Band s t a) -> GDAL s b) -> GDAL s b
unsafeWithBand (Band p) f = f p

type ROBand s = Band s ReadOnly
type RWBand s = Band s ReadWrite


{#pointer GDALDriverH as DriverH newtype#}

{#fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{#fun GDALDestroyDriverManager as destroyDriverManager {} -> `()'#}

{# fun unsafe GDALGetDriverByName as c_driverByName
    { `String' } -> `DriverH' id #}

driverByName :: Driver -> GDAL s DriverH
driverByName s = GDAL $ liftIO $ do
    driver@(DriverH ptr) <- c_driverByName (show s)
    if ptr==nullPtr
        then throw DriverLoadError
        else return driver

type DriverOptions = [(String,String)]

create
  :: forall s a. GDALType a
  => Driver -> String -> Int -> Int -> Int -> DriverOptions
  -> GDAL s (Dataset s ReadWrite a)
create drv path nx ny bands options = do
  d <- driverByName drv
  ptr <- liftIO $ withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC $ datatype (Proxy :: Proxy a)
    withOptionList options $ \opts ->
        c_create d path' nx' ny' bands' dtype' opts
  newDatasetHandle ptr

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
  -> IO (Ptr (RWDataset s a))

openReadOnly :: GDALType a => FilePath -> GDAL s (RODataset s a)
openReadOnly p = openWithMode GA_ReadOnly p >>= checkType

openReadWrite :: GDALType a => FilePath -> GDAL s (RWDataset s a)
openReadWrite p = openWithMode GA_Update p >>= checkType

openWithMode :: Access -> String -> GDAL s (Dataset s t a)
openWithMode m p =
  liftIO (withCString p $ \p' -> open_ p' (fromEnumC m)) >>= newDatasetHandle

openAnyWithMode :: forall s t. Access -> String -> GDAL s (SomeDataset s t)
openAnyWithMode m p = do
  ds <- openWithMode m p
  dt <- getBand ds 1 >>= bandDatatype
  case dt of
    GDT_Byte     -> return $ SomeDataset ((coerce ds) :: Dataset s t Word8)
    GDT_UInt16   -> return $ SomeDataset ((coerce ds) :: Dataset s t Word16)
    GDT_UInt32   -> return $ SomeDataset ((coerce ds) :: Dataset s t Word32)
    GDT_Int16    -> return $ SomeDataset ((coerce ds) :: Dataset s t Int16)
    GDT_Int32    -> return $ SomeDataset ((coerce ds) :: Dataset s t Int32)
    GDT_Float32  -> return $ SomeDataset ((coerce ds) :: Dataset s t Float)
    GDT_Float64  -> return $ SomeDataset ((coerce ds) :: Dataset s t Double)
    GDT_CInt16   -> return $ SomeDataset ((coerce ds) :: Dataset s t (Complex Int16))
    GDT_CInt32   -> return $ SomeDataset ((coerce ds) :: Dataset s t (Complex Int32))
    GDT_CFloat32 -> return $ SomeDataset ((coerce ds) :: Dataset s t (Complex Float))
    GDT_CFloat64 -> return $ SomeDataset ((coerce ds) :: Dataset s t (Complex Double))
    _            -> liftIO (throw InvalidType)

checkType
  :: forall s t a. GDALType a
  => Dataset s t a -> GDAL s (Dataset s t a)
checkType ds = do
  c <- datasetBandCount ds
  if c > 0
    then do
      b <- getBand ds 1
      dt <- bandDatatype b
      if datatype (Proxy :: Proxy a) == dt
        then return ds
        else liftIO (throw InvalidType)
    else return ds

data SomeDataset s (t :: DatasetMode)
  = forall a. GDALType a => SomeDataset (Dataset s t a)

openAnyReadOnly :: forall s. FilePath -> GDAL s (SomeDataset s ReadOnly)
openAnyReadOnly = openAnyWithMode GA_ReadOnly

openAnyReadWrite :: forall s. FilePath -> GDAL s (SomeDataset s ReadWrite)
openAnyReadWrite = openAnyWithMode GA_Update

foreign import ccall safe "gdal.h GDALOpen" open_
   :: CString -> CInt -> IO (Ptr (Dataset s t a))


createCopy' :: GDALType a
  => Driver -> String -> (Dataset s t a) -> Bool -> DriverOptions
  -> ProgressFun -> GDAL s (RWDataset s a)
createCopy' driver path dataset strict options progressFun = do
  d <- driverByName driver
  ptr <- unsafeWithDatasetIO dataset $ \ds -> do
    withCString path $ \p ->
      withProgressFun progressFun $ \pFunc -> do
          let s = fromBool strict
          withOptionList options $ \o ->
              c_createCopy d p ds s o pFunc (castPtr nullPtr)
  newDatasetHandle ptr

createCopy :: GDALType a
  => Driver -> FilePath -> (Dataset s t a) -> Bool -> DriverOptions
  -> GDAL s (RWDataset s a)
createCopy driver path dataset strict options = do
  createCopy' driver path dataset strict options (\_ _ _ -> return 1)

foreign import ccall safe "gdal.h GDALCreateCopy" c_createCopy
  :: DriverH
  -> Ptr CChar
  -> Ptr (Dataset s t a)
  -> CInt
  -> Ptr CString
  -> FunPtr ProgressFun
  -> Ptr ()
  -> IO (Ptr (RWDataset s a))


withProgressFun :: forall c.
  ProgressFun -> (FunPtr ProgressFun -> IO c) -> IO c
withProgressFun = withCCallback wrapProgressFun

withCCallback :: forall c t a.
  (t -> IO (FunPtr a)) -> t -> (FunPtr a -> IO c) -> IO c
withCCallback w f = bracket (w f) freeHaskellFunPtr

type ProgressFun = CDouble -> Ptr CChar -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  wrapProgressFun :: ProgressFun -> IO (FunPtr ProgressFun)


newDatasetHandle :: Ptr (Dataset s t a) -> GDAL s (Dataset s t a)
newDatasetHandle p = GDAL $
    if p==nullPtr
        then liftIO $ throw NullDatasetHandle
        else do _ <- register (c_closeDataset p)
                return $ Dataset p

foreign import ccall "gdal.h GDALClose"
  c_closeDataset :: Ptr (Dataset s t a) -> IO ()

createMem
  :: GDALType a
  => Int -> Int -> Int -> DriverOptions -> GDAL s (Dataset s ReadWrite a)
createMem = create MEM ""

flushCache :: forall s a. RWDataset s a -> GDAL s ()
flushCache = flip unsafeWithDatasetIO flushCache'

foreign import ccall safe "gdal.h GDALFlushCache" flushCache'
  :: Ptr (Dataset s t a) -> IO ()

datasetSize :: Dataset s t a -> GDAL s (Int, Int)
datasetSize ds = unsafeWithDatasetIO ds $ \dPtr -> do
  x <- getDatasetXSize_ dPtr
  y <- getDatasetYSize_ dPtr
  return (fromIntegral x, fromIntegral y)

foreign import ccall unsafe "gdal.h GDALGetRasterXSize" getDatasetXSize_
  :: Ptr (Dataset s t a) -> IO CInt

foreign import ccall unsafe "gdal.h GDALGetRasterYSize" getDatasetYSize_
  :: Ptr (Dataset s t a) -> IO CInt

datasetProjection :: Dataset s t a -> GDAL s String
datasetProjection = flip unsafeWithDatasetIO (\d -> getProjection_ d >>= peekCString)

foreign import ccall unsafe "gdal.h GDALGetProjectionRef" getProjection_
  :: Ptr (Dataset s t a) -> IO (Ptr CChar)


setDatasetProjection :: (RWDataset s a) -> String -> GDAL s ()
setDatasetProjection d p = unsafeWithDatasetIO d $ \d' ->
  throwIfError "setDatasetProjection: could not set projection" $
    withCString p $ \p' -> setProjection' d' p'

foreign import ccall unsafe "gdal.h GDALSetProjection" setProjection'
  :: Ptr (Dataset s t a) -> Ptr CChar -> IO CInt


data Geotransform = Geotransform !Double !Double !Double !Double !Double !Double
    deriving (Eq, Show)

datasetGeotransform :: Dataset s t a -> GDAL s Geotransform
datasetGeotransform d = unsafeWithDatasetIO d $ \dPtr ->
  allocaArray 6 $ \a -> do
    throwIfError "could not get geotransform" $ getGeoTransform dPtr a
    Geotransform <$> liftM realToFrac (peekElemOff a 0)
                 <*> liftM realToFrac (peekElemOff a 1)
                 <*> liftM realToFrac (peekElemOff a 2)
                 <*> liftM realToFrac (peekElemOff a 3)
                 <*> liftM realToFrac (peekElemOff a 4)
                 <*> liftM realToFrac (peekElemOff a 5)

foreign import ccall unsafe "gdal.h GDALGetGeoTransform" getGeoTransform
  :: Ptr (Dataset s t a) -> Ptr CDouble -> IO CInt

setDatasetGeotransform :: (RWDataset s a) -> Geotransform -> GDAL s ()
setDatasetGeotransform ds gt = unsafeWithDatasetIO ds $ \dPtr ->
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
  :: Ptr (Dataset s t a) -> Ptr CDouble -> IO CInt

datasetBandCount :: Dataset s t a -> GDAL s Int
datasetBandCount = flip unsafeWithDatasetIO (fmap fromIntegral . bandCount_)

foreign import ccall unsafe "gdal.h GDALGetRasterCount" bandCount_
  :: Ptr (Dataset s t a) -> IO CInt

getBand :: Dataset s t a -> Int -> GDAL s (Band s t a)
getBand ds band = unsafeWithDatasetIO ds $ \dPtr -> do
    rBand@(Band p) <- getRasterBand dPtr (fromIntegral band)
    if p == nullPtr
        then throw (InvalidBand band)
        else return rBand

foreign import ccall unsafe "gdal.h GDALGetRasterBand" getRasterBand
  :: Ptr (Dataset s t a) -> CInt -> IO ((Band s t a))


bandDatatype :: (Band s t a) -> GDAL s Datatype
bandDatatype = fmap toEnumC . liftIO . getDatatype_

foreign import ccall unsafe "gdal.h GDALGetRasterDataType" getDatatype_
  :: (Band s a t) -> IO CInt


bandBlockSize :: (Band s t a) -> GDAL s (Int,Int)
bandBlockSize band = liftIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   getBlockSize_ band xPtr yPtr
   liftA2 (,) (liftM fromIntegral $ peek xPtr) (liftM fromIntegral $ peek yPtr)

foreign import ccall unsafe "gdal.h GDALGetBlockSize" getBlockSize_
    :: (Band s a t) -> Ptr CInt -> Ptr CInt -> IO ()


bandBlockLen :: (Band s t a) -> GDAL s Int
bandBlockLen = fmap (uncurry (*)) . bandBlockSize

bandSize :: (Band s a t) -> GDAL s (Int, Int)
bandSize band = liftIO $ do
  x <- getBandXSize_ band
  y <- getBandYSize_ band
  return (fromIntegral x, fromIntegral y)

bandBlockCount :: Band s t a -> GDAL s (Int, Int)
bandBlockCount b = do
  (nx,ny) <- bandSize b
  (bx,by) <- bandBlockSize b
  return ( ceiling (fromIntegral nx / fromIntegral bx :: Double)
         , ceiling (fromIntegral ny / fromIntegral by :: Double))

foreign import ccall unsafe "gdal.h GDALGetRasterBandXSize" getBandXSize_
  :: (Band s t a) -> IO CInt

foreign import ccall unsafe "gdal.h GDALGetRasterBandYSize" getBandYSize_
  :: (Band s t a) -> IO CInt


bandNodataValue :: (Band s t a) -> GDAL s (Maybe Double)
bandNodataValue = fmap (fmap realToFrac) . liftIO . c_bandNodataValue

c_bandNodataValue :: (Band s t a) -> IO (Maybe CDouble)
c_bandNodataValue b = alloca $ \p -> do
   value <- getNodata_ b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
{-# INLINE c_bandNodataValue #-}
   
foreign import ccall unsafe "gdal.h GDALGetRasterNoDataValue" getNodata_
   :: (Band s t a) -> Ptr CInt -> IO CDouble
   

setBandNodataValue :: (RWBand s a) -> Double -> GDAL s ()
setBandNodataValue b v = liftIO $ throwIfError "could not set nodata" $
                            setNodata_ b (realToFrac v)

foreign import ccall safe "gdal.h GDALSetRasterNoDataValue" setNodata_
    :: (RWBand s t) -> CDouble -> IO CInt

fillBand :: (RWBand s a) -> Double -> Double -> GDAL s ()
fillBand b r i = liftIO $ throwIfError "could not fill band" $
    fillRaster_ b (realToFrac r) (realToFrac i)

foreign import ccall safe "gdal.h GDALFillRaster" fillRaster_
    :: (RWBand s t) -> CDouble -> CDouble -> IO CInt


readBand :: forall s t a. GDALType a
  => (Band s t a)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> GDAL s (Vector (Value a))
readBand band xoff yoff sx sy bx by = liftIO $
  readBandIO band xoff yoff sx sy bx by
{-# INLINE readBand #-}

-- | Unsafe lazy IO version of readBand.
--   *must* make sure vectors are evaluated inside the GDAL monad and in the
--   same thread that called 'runGDAL0
unsafeLazyReadBand :: forall s a. GDALType a
  => (ROBand s a)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> GDAL s (Vector (Value a))
unsafeLazyReadBand band xoff yoff sx sy bx by = liftIO $
  unsafeInterleaveIO $ readBandIO band xoff yoff sx sy bx by
{-# INLINE unsafeLazyReadBand #-}

readBandIO :: forall s t a. GDALType a
  => (Band s t a)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> IO (Vector (Value a))
readBandIO band xoff yoff sx sy bx by =
  readMasked band $ \(b :: Band s t a') -> do
    fp <- mallocForeignPtrArray (bx * by)
    let dtype = fromEnumC (datatype (Proxy :: Proxy a'))
    withForeignPtr fp $ \ptr -> do
      throwIfError "could not advise read" $
        adviseRead_
          b
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
          b
          (fromEnumC GF_Read) 
          (fromIntegral xoff)
          (fromIntegral yoff)
          (fromIntegral sx)
          (fromIntegral sy)
          (castPtr ptr)
          (fromIntegral bx)
          (fromIntegral by)
          dtype
          0
          0
    return $ St.unsafeFromForeignPtr0 fp (bx * by)
{-# INLINE readBandIO #-}

readMasked
  :: GDALType a
  => Band s t a
  -> (forall a'. GDALType a' => Band s t a' -> IO (Vector a'))
  -> IO (Vector (Value a))
readMasked band reader
  | hasFlag MaskPerDataset = useMaskBand
  | hasFlag MaskNoData     = useNoData
  | hasFlag MaskAllValid   = useAsIs
  | otherwise              = useMaskBand
  where
    hasFlag f = fromEnumC f .&. c_getMaskFlags band == fromEnumC f
    useAsIs = fmap (St.map Value) (reader band)
    useNoData = do
      mNodata <- c_bandNodataValue band
      let toValue = case mNodata of
                      Nothing -> Value
                      Just nd ->
                        \v -> if toNodata v == nd then NoData else Value v
      fmap (St.map toValue) (reader band)
    useMaskBand = do
      vs <- reader band
      mask <- c_getMaskBand band
      ms <- reader mask
      return $ St.zipWith (\v m -> if m/=0 then Value v else NoData) vs ms
{-# INLINE readMasked #-}

foreign import ccall safe "gdal.h GDALRasterAdviseRead" adviseRead_
    :: (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
    -> Ptr (Ptr CChar) -> IO CInt


writeBand :: forall s a. GDALType a
  => (RWBand s a)
  -> Int -> Int
  -> Int -> Int
  -> Int -> Int
  -> Vector (Value a)
  -> GDAL s ()
writeBand band xoff yoff sx sy bx by uvec = do
  bNodata <- liftIO $ fmap (maybe nodata fromNodata) (c_bandNodataValue band)
  liftIO $ do
    let nElems    = bx * by
        (fp, len) = St.unsafeToForeignPtr0 vec
        vec = St.map (fromValue bNodata) uvec
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
            0
            0
{-# INLINE writeBand #-}

foreign import ccall safe "gdal.h GDALRasterIO" rasterIO_
  :: (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> CInt -> CInt
  -> CInt -> CInt -> CInt -> IO CInt

readBandBlock
  :: forall s t a. GDALType a
  => Band s t a -> Int -> Int -> GDAL s (Vector (Value a))
readBandBlock band x y = do
  len <- bandBlockLen band
  liftIO $ readMasked band $ \b -> do
    f <- mallocForeignPtrArray len
    withForeignPtr f $ \ptr ->
      throwIfError "could not read block" $
        readBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)
    return $ St.unsafeFromForeignPtr0 f len
{-# INLINE readBandBlock #-}

foreign import ccall safe "gdal.h GDALReadBlock" readBlock_
    :: (Band s a t) -> CInt -> CInt -> Ptr () -> IO CInt

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> Int -> Int -> Vector (Value a) -> GDAL s ()
writeBandBlock b x y uvec = do
  nElems <- bandBlockLen b
  bNodata <- liftIO $ fmap (maybe nodata fromNodata) (c_bandNodataValue b)
  liftIO $ do
    let (fp, len) = St.unsafeToForeignPtr0 vec
        vec = St.map (fromValue bNodata) uvec
    if nElems /= len
      then throw InvalidBlockSize
      else withForeignPtr fp $ \ptr ->
           throwIfError "could not write block" $
              writeBlock_ b (fromIntegral x) (fromIntegral y)
                            (castPtr ptr)
{-# INLINE writeBandBlock #-}

foreign import ccall safe "gdal.h GDALWriteBlock" writeBlock_
   :: (RWBand s a) -> CInt -> CInt -> Ptr () -> IO CInt

foreign import ccall safe "gdal.h GDALGetMaskBand" c_getMaskBand
   :: (Band s t a) -> IO (Band s t Word8)

#c
enum MaskFlag {
     MaskAllValid = GMF_ALL_VALID,
     MaskPerDataset = GMF_PER_DATASET,
     MaskAlpha = GMF_ALPHA,
     MaskNoData = GMF_NODATA
};
#endc

{# enum MaskFlag  {} deriving (Eq, Bounded, Show) #}

foreign import ccall unsafe "gdal.h GDALGetMaskFlags" c_getMaskFlags
   :: (Band s t a) -> CInt


instance GDALType Word8 where
  datatype _ = GDT_Byte
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = round
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Word16 where
  datatype _ = GDT_UInt16
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = round
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Word32 where
  datatype _ = GDT_UInt32
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = round
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Int16 where
  datatype _ = GDT_Int16
  nodata = minBound
  toNodata = fromIntegral
  fromNodata = round
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Int32 where
  datatype _ = GDT_Int32
  nodata = minBound
  toNodata = fromIntegral
  fromNodata = round
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Float where
  datatype _ = GDT_Float32
  nodata = 0/0
  toNodata = realToFrac
  fromNodata = realToFrac
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Double where
  datatype _ = GDT_Float64
  nodata = 0/0
  toNodata = realToFrac
  fromNodata = realToFrac
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType (Complex Int16) where
  datatype _ = GDT_CInt16
  nodata = nodata :+ nodata
  toNodata = fromIntegral . realPart
  fromNodata d = fromNodata d :+ fromNodata d
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType (Complex Int32) where
  datatype _ = GDT_CInt32
  nodata = nodata :+ nodata
  toNodata = fromIntegral . realPart
  fromNodata d = fromNodata d :+ fromNodata d
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType (Complex Float) where
  datatype _ = GDT_CFloat32
  nodata = nodata :+ nodata
  toNodata = realToFrac . realPart
  fromNodata d = fromNodata d :+ fromNodata d
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType (Complex Double) where
  datatype _ = GDT_CFloat64
  nodata = nodata :+ nodata
  toNodata = realToFrac . realPart
  fromNodata d = fromNodata d :+ fromNodata d
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}
