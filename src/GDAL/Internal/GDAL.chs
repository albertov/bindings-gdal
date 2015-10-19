{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.GDAL (
    GDALType (..)
  , GDALRasterException (..)
  , Datatype (..)
  , Geotransform (..)
  , Driver (..)
  , Dataset
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , registerAllDrivers
  , destroyDriverManager
  , create
  , createMem
  , flushCache
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy
  , driverCreationOptionList

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
  , bandBlockLen
  , bandSize
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , readBand
  , readBandPure
  , readBandBlock
  , writeBand
  , writeBandBlock
  , fillBand

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'

  , unDataset
  , unBand
  , withLockedDatasetPtr
  , withLockedBandPtr
  , newDerivedDatasetHandle
) where

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..))
import Control.Monad (liftM, liftM2, when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Int (Int16, Int32)
import Data.Bits ((.&.))
import Data.Complex (Complex(..), realPart)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Storable as St
import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool, fromBool)

import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Data.Char (toUpper)

import GDAL.Internal.Types
import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.OSR (SpatialReference, fromWkt, toWkt)
import GDAL.Internal.OGRError (OGRException)
import GDAL.Internal.Util


#include "gdal.h"


$(let names = fmap (words . map toUpper) $
                readProcess "gdal-config" ["--formats"] ""
  in createEnum "Driver" names)

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDatatype   !Datatype
  | InvalidProjection !OGRException
  | InvalidDriverOptions
  | NullDataset
  | CopyStopped
  deriving (Typeable, Show, Eq)

instance NFData GDALRasterException where
  rnf a = a `seq` () -- All fields are already strict so no need to rnf them

instance Exception GDALRasterException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException


class (Storable a, Show a) => GDALType a where
  datatype :: Proxy a -> Datatype
  -- | default nodata value when writing to bands with no datavalue set
  nodata   :: a
  -- | how to convert to double for use in setBandNodataValue
  toNodata :: a -> CDouble
  -- | how to convert from double for use with bandNodataValue
  fromNodata :: CDouble -> a

{# enum GDALDataType as Datatype {upcaseFirstLetter} deriving (Eq) #}

instance NFData Datatype where
  rnf a = a `seq`()

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


newtype Dataset s (t::AccessMode) a
  = Dataset (Mutex, Ptr (Dataset s t a))

unDataset :: Dataset s t a -> Ptr (Dataset s t a)
unDataset (Dataset (_,p)) = p

withLockedDatasetPtr
  :: Dataset s t a -> (Ptr (Dataset s t a) -> IO b) -> IO b
withLockedDatasetPtr (Dataset (m,p)) f = withMutex m (f p)

type RODataset s = Dataset s ReadOnly
type RWDataset s = Dataset s ReadWrite

{#pointer GDALRasterBandH as Band newtype nocode#}

newtype (Band s (t::AccessMode) a)
  = Band (Mutex, Ptr (Band s t a))

unBand :: Band s t a -> Ptr (Band s t a)
unBand (Band (_,p)) = p

withLockedBandPtr
  :: Band s t a -> (Ptr (Band s t a) -> IO b) -> IO b
withLockedBandPtr (Band (m,p)) f = withMutex m (f p)

type ROBand s = Band s ReadOnly
type RWBand s = Band s ReadWrite


{#pointer GDALDriverH as DriverH newtype#}

{#fun GDALAllRegister as registerAllDrivers {} -> `()'  #}

{#fun GDALDestroyDriverManager as destroyDriverManager {} -> `()'#}

{# fun unsafe GDALGetDriverByName as c_driverByName
    { `String' } -> `DriverH' id #}

driverByName :: Driver -> IO DriverH
driverByName s = throwIfError "driverByName" (c_driverByName (show s))

driverCreationOptionList :: Driver -> String
driverCreationOptionList driver = unsafePerformIO $ do
  d <- driverByName driver
  {#call GDALGetDriverCreationOptionList	as ^#} d >>= peekCString

validateCreationOptions :: DriverH -> Ptr CString -> IO ()
validateCreationOptions d o = do
  valid <- liftM toBool ({#call unsafe GDALValidateCreationOptions as ^ #} d o)
  when (not valid) (throwBindingException InvalidDriverOptions)

create
  :: forall s a. GDALType a
  => Driver -> String -> Size -> Int -> OptionList
  -> GDAL s (Dataset s ReadWrite a)
create drv path (XY nx ny) bands options = do
  ptr <- liftIO $ withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC $ datatype (Proxy :: Proxy a)
    d <- driverByName drv
    withOptionList options $ \opts -> do
      validateCreationOptions d opts
      c_create d path' nx' ny' bands' dtype' opts
  newDatasetHandle ptr

foreign import ccall safe "gdal.h GDALCreate" c_create
  :: DriverH
  -> CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CString
  -> IO (Ptr (RWDataset s a))

openReadOnly :: String -> GDAL s (RODataset s a)
openReadOnly p = openWithMode GA_ReadOnly p

openReadWrite :: GDALType a => String -> GDAL s (RWDataset s a)
openReadWrite p = openWithMode GA_Update p

openWithMode :: Access -> String -> GDAL s (Dataset s t a)
openWithMode m p =
  (liftIO $ withCString p $ \p' -> throwIfError "open" (open_ p' (fromEnumC m)))
  >>= newDatasetHandle

unsafeToReadOnly :: RWDataset s a -> GDAL s (RODataset s a)
unsafeToReadOnly ds = flushCache ds >> return (coerce ds)

checkType
  :: forall s t a. GDALType a
  => Band s t a -> GDAL s ()
checkType b
  | rt == bt  = return ()
  | otherwise = throwBindingException (InvalidDatatype bt)
  where rt = datatype (Proxy :: Proxy a) 
        bt = bandDatatype b

foreign import ccall safe "gdal.h GDALOpen" open_
   :: CString -> CInt -> IO (Ptr (Dataset s t a))


createCopy :: GDALType a
  => Driver -> String -> (Dataset s t a) -> Bool -> OptionList
  -> Maybe ProgressFun -> GDAL s (RWDataset s a)
createCopy driver path ds strict options progressFun = do
  mPtr <- liftIO $ do
    d <- driverByName driver
    throwIfError "createCopy" $
      withCString path $ \p ->
      withProgressFun progressFun $ \pFunc ->
      withOptionList options $ \o -> do
        validateCreationOptions d o
        withLockedDatasetPtr ds $ \dsPtr ->
          c_createCopy d p dsPtr (fromBool strict) o pFunc (castPtr nullPtr)
  maybe (throwBindingException CopyStopped) newDatasetHandle mPtr

foreign import ccall safe "gdal.h GDALCreateCopy" c_createCopy
  :: DriverH
  -> Ptr CChar
  -> Ptr (Dataset s t a)
  -> CInt
  -> Ptr CString
  -> ProgressFunPtr
  -> Ptr ()
  -> IO (Ptr (RWDataset s a))


newDatasetHandle :: Ptr (Dataset s t a) -> GDAL s (Dataset s t a)
newDatasetHandle p
  | p==nullPtr  = throwBindingException NullDataset
  | otherwise   = do
      registerFinalizer (safeCloseDataset p)
      m <- liftIO newMutex
      return $ Dataset (m,p)

newDerivedDatasetHandle
  :: Dataset s t' a -> Ptr (Dataset s t b) -> GDAL s (Dataset s t b)
newDerivedDatasetHandle (Dataset (m,_)) p
  | p==nullPtr = throwBindingException NullDataset
  | otherwise  = do
      registerFinalizer (safeCloseDataset p)
      return $ Dataset (m,p)

safeCloseDataset :: Ptr (Dataset s t a) -> IO ()
safeCloseDataset p = do
  count <- c_dereferenceDataset p
  when (count < 1) $ (c_referenceDataset p >> c_closeDataset p)

foreign import ccall "gdal.h GDALReferenceDataset"
  c_referenceDataset :: Ptr (Dataset s t a) -> IO CInt

foreign import ccall "gdal.h GDALDereferenceDataset"
  c_dereferenceDataset :: Ptr (Dataset s t a) -> IO CInt

foreign import ccall "gdal.h GDALClose"
  c_closeDataset :: Ptr (Dataset s t a) -> IO ()

createMem
  :: GDALType a
  => Size -> Int -> OptionList -> GDAL s (Dataset s ReadWrite a)
createMem = create MEM ""

flushCache :: forall s a. RWDataset s a -> GDAL s ()
flushCache = liftIO . flip withLockedDatasetPtr c_flushCache

foreign import ccall safe "gdal.h GDALFlushCache" c_flushCache
  :: Ptr (Dataset s t a) -> IO ()

datasetSize :: Dataset s t a -> (Int, Int)
datasetSize ds
  = let d = unDataset ds
    in (fromIntegral (getDatasetXSize_ d), fromIntegral (getDatasetYSize_ d)) 

foreign import ccall unsafe "gdal.h GDALGetRasterXSize" getDatasetXSize_
  :: Ptr (Dataset s t a) -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterYSize" getDatasetYSize_
  :: Ptr (Dataset s t a) -> CInt

datasetProjection :: Dataset s t a -> GDAL s (Maybe SpatialReference)
datasetProjection d = do
  srs <- liftIO (getProjection_ (unDataset d) >>= peekCString)
  if null srs
    then return Nothing
    else either (throwBindingException . InvalidProjection)
                (return . Just)
                (fromWkt srs)

foreign import ccall unsafe "gdal.h GDALGetProjectionRef" getProjection_
  :: Ptr (Dataset s t a) -> IO (Ptr CChar)


setDatasetProjection :: RWDataset s a -> SpatialReference -> GDAL s ()
setDatasetProjection d srs = liftIO $
  throwIfError_ "setDatasetProjection" $
    withLockedDatasetPtr d $ withCString (toWkt srs) . setProjection' 

foreign import ccall unsafe "gdal.h GDALSetProjection" setProjection'
  :: Ptr (Dataset s t a) -> Ptr CChar -> IO CInt


data Geotransform
  = Geotransform {
      gtXOff   :: !Double
    , gtXDelta :: !Double
    , gtXRot   :: !Double
    , gtYOff   :: !Double
    , gtYRot   :: !Double
    , gtYDelta :: !Double
  } deriving (Eq, Show)

instance Storable Geotransform where
  sizeOf _     = sizeOf (undefined :: CDouble) * 6
  alignment _  = alignment (undefined :: CDouble)
  poke pGt (Geotransform g0 g1 g2 g3 g4 g5) = do
    let p = castPtr pGt :: Ptr CDouble
    pokeElemOff p 0 (realToFrac g0)
    pokeElemOff p 1 (realToFrac g1)
    pokeElemOff p 2 (realToFrac g2)
    pokeElemOff p 3 (realToFrac g3)
    pokeElemOff p 4 (realToFrac g4)
    pokeElemOff p 5 (realToFrac g5)
  peek pGt = do
    let p = castPtr pGt :: Ptr CDouble
    Geotransform <$> liftM realToFrac (peekElemOff p 0)
                 <*> liftM realToFrac (peekElemOff p 1)
                 <*> liftM realToFrac (peekElemOff p 2)
                 <*> liftM realToFrac (peekElemOff p 3)
                 <*> liftM realToFrac (peekElemOff p 4)
                 <*> liftM realToFrac (peekElemOff p 5)
            

datasetGeotransform :: Dataset s t a -> GDAL s Geotransform
datasetGeotransform d = liftIO $ alloca $ \p -> do
  throwIfError_ "datasetGeotransform" (getGeoTransform (unDataset d) p)
  peek (castPtr p)

foreign import ccall unsafe "gdal.h GDALGetGeoTransform" getGeoTransform
  :: Ptr (Dataset s t a) -> Ptr CDouble -> IO CInt

setDatasetGeotransform :: (RWDataset s a) -> Geotransform -> GDAL s ()
setDatasetGeotransform ds gt = liftIO $
  throwIfError_ "setDatasetGeotransform" $
    withLockedDatasetPtr ds $ \dsPtr ->
      alloca $ \p -> (poke p gt >> setGeoTransform dsPtr (castPtr p))


foreign import ccall unsafe "gdal.h GDALSetGeoTransform" setGeoTransform
  :: Ptr (Dataset s t a) -> Ptr CDouble -> IO CInt

datasetBandCount :: Dataset s t a -> Int
datasetBandCount = fromIntegral . bandCount_ . unDataset

foreign import ccall unsafe "gdal.h GDALGetRasterCount" bandCount_
  :: Ptr (Dataset s t a) -> CInt

getBand :: Int -> Dataset s t a -> GDAL s (Band s t a)
getBand band (Dataset (m,dp)) = liftIO $ do
  p <- throwIfError "getBand" (c_getRasterBand dp (fromIntegral band))
  return (Band (m,p))

foreign import ccall safe "gdal.h GDALGetRasterBand" c_getRasterBand
  :: Ptr (Dataset s t a) -> CInt -> IO (Ptr (Band s t a))


bandDatatype :: Band s t a -> Datatype
bandDatatype = toEnumC . c_getRasterDataType . unBand

foreign import ccall unsafe "gdal.h GDALGetRasterDataType" c_getRasterDataType
  :: Ptr (Band s a t) -> CInt


bandBlockSize :: (Band s t a) -> Size
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   getBlockSize_ (unBand band) xPtr yPtr
   liftM (fmap fromIntegral) (liftM2 XY (peek xPtr) (peek yPtr))

foreign import ccall unsafe "gdal.h GDALGetBlockSize" getBlockSize_
    :: Ptr (Band s a t) -> Ptr CInt -> Ptr CInt -> IO ()


bandBlockLen :: Band s t a -> Int
bandBlockLen = (\(XY x y) -> x*y) . bandBlockSize

bandSize :: Band s a t -> Size
bandSize b = fmap fromIntegral $
               XY (getBandXSize_ (unBand b)) (getBandYSize_ (unBand b))

allBand :: Band s a t -> Window Int
allBand = Window (pure 0) . bandSize

bandBlockCount :: Band s t a -> XY Int
bandBlockCount b = fmap ceiling $ liftA2 ((/) :: Double -> Double -> Double)
                     (fmap fromIntegral (bandSize b))
                     (fmap fromIntegral (bandBlockSize b))

foreign import ccall unsafe "gdal.h GDALGetRasterBandXSize" getBandXSize_
  :: Ptr (Band s t a) -> CInt

foreign import ccall unsafe "gdal.h GDALGetRasterBandYSize" getBandYSize_
  :: Ptr (Band s t a) -> CInt


bandNodataValue :: GDALType a => (Band s t a) -> GDAL s (Maybe a)
bandNodataValue = fmap (fmap fromNodata) . liftIO . c_bandNodataValue . unBand

c_bandNodataValue :: Ptr (Band s t a) -> IO (Maybe CDouble)
c_bandNodataValue b = alloca $ \p -> do
   value <- getNodata_ b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
{-# INLINE c_bandNodataValue #-}
   
foreign import ccall unsafe "gdal.h GDALGetRasterNoDataValue" getNodata_
   :: Ptr (Band s t a) -> Ptr CInt -> IO CDouble
   

setBandNodataValue :: GDALType a => (RWBand s a) -> a -> GDAL s ()
setBandNodataValue b v
  = liftIO $ throwIfError_ "setBandNodataValue" $
      setNodata_ (unBand b) (toNodata v)

foreign import ccall safe "gdal.h GDALSetRasterNoDataValue" setNodata_
    :: Ptr (RWBand s t) -> CDouble -> IO CInt

fillBand :: (RWBand s a) -> Double -> Double -> GDAL s ()
fillBand b r i = liftIO $ throwIfError_ "fillBand" $
    fillRaster_ (unBand b) (realToFrac r) (realToFrac i)

foreign import ccall safe "gdal.h GDALFillRaster" fillRaster_
    :: Ptr (RWBand s t) -> CDouble -> CDouble -> IO CInt


readBand :: forall s t b a. GDALType a
  => (Band s t b)
  -> Window Int
  -> Size
  -> GDAL s (Vector (Value a))
readBand band win size = liftIO $ readBandIO band win size
{-# INLINE readBand #-}

readBandPure :: forall s b a. GDALType a
  => (ROBand s b)
  -> Window Int
  -> Size
  -> Vector (Value a)
readBandPure band win size = unsafePerformIO $ readBandIO band win size
{-# INLINE readBandPure #-}

readBandIO :: forall s t b a. GDALType a
  => (Band s t b)
  -> Window Int
  -> Size
  -> IO (Vector (Value a))
readBandIO band win (XY bx by) = readMasked band read_
  where
    XY sx sy     = winSize win
    XY xoff yoff = winMin win
    read_ :: forall b' a'. GDALType a' => Ptr (Band s t b') -> IO (St.Vector a')
    read_ b = do
      fp <- mallocForeignPtrArray (bx * by)
      let dtype = fromEnumC (datatype (Proxy :: Proxy a'))
      withForeignPtr fp $ \ptr -> do
        throwIfError_ "readBandIO" $ do
          e <- adviseRead_
            b
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            (fromIntegral bx)
            (fromIntegral by)
            dtype
            (castPtr nullPtr)
          if (toEnumC e == CE_None)
            then rasterIO_
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
            else return e
      return $ St.unsafeFromForeignPtr0 fp (bx * by)
{-# INLINE readBandIO #-}

readMasked
  :: GDALType a
  => Band s t b
  -> (forall a' b'. GDALType a' => Ptr (Band s t b') -> IO (St.Vector a'))
  -> IO (Vector (Value a))
readMasked band reader = withLockedBandPtr band $ \bPtr -> do
  flags <- c_getMaskFlags bPtr
  reader bPtr >>= fmap stToUValue . mask flags bPtr
  where
    mask fs
      | hasFlag fs MaskPerDataset = useMaskBand
      | hasFlag fs MaskNoData     = useNoData
      | hasFlag fs MaskAllValid   = useAsIs
      | otherwise                 = useMaskBand
    hasFlag fs f = fromEnumC f .&. fs == fromEnumC f
    useAsIs _ = return . St.map Value
    useNoData bPtr vs = do
      mNodata <- c_bandNodataValue bPtr
      let toValue = case mNodata of
                      Nothing -> Value
                      Just nd ->
                        \v -> if toNodata v == nd then NoData else Value v
      return (St.map toValue vs)
    useMaskBand bPtr vs = do
      ms <- c_getMaskBand bPtr >>= reader :: IO (St.Vector Word8)
      return $ St.zipWith (\v m -> if m/=0 then Value v else NoData) vs ms
{-# INLINE readMasked #-}

foreign import ccall safe "gdal.h GDALRasterAdviseRead" adviseRead_
    :: Ptr (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt
    -> Ptr (Ptr CChar) -> IO CInt

writeBand :: forall s b a. GDALType a
  => RWBand s b
  -> Window Int
  -> Size
  -> Vector (Value a)
  -> GDAL s ()
writeBand band win sz@(XY bx by) uvec = liftIO $
  withLockedBandPtr band $ \bPtr -> do
    bNodata <- fmap (maybe nodata fromNodata) (c_bandNodataValue bPtr)
    let nElems    = bx * by
        (fp, len) = St.unsafeToForeignPtr0 vec
        vec       = St.map (fromValue bNodata) (uToStValue uvec)
        XY sx sy     = winSize win
        XY xoff yoff = winMin win
    if nElems /= len
      then throwBindingException (InvalidRasterSize sz)
      else withForeignPtr fp $ \ptr -> do
        throwIfError_ "writeBand" $
          rasterIO_
            bPtr
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
  :: Ptr (Band s a t) -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr ()
  -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

readBandBlock
  :: forall s t a. GDALType a
  => Band s t a -> BlockIx -> GDAL s (Vector (Value a))
readBandBlock band (XY x y) = do
  checkType band
  liftIO $ readMasked band $ \b -> do
    f <- mallocForeignPtrArray len
    withForeignPtr f $ \ptr ->
      throwIfError_ "readBandBlock" $
        readBlock_ b (fromIntegral x) (fromIntegral y) (castPtr ptr)
    return $ St.unsafeFromForeignPtr0 f len
  where len = bandBlockLen band
{-# INLINE readBandBlock #-}

foreign import ccall safe "gdal.h GDALReadBlock" readBlock_
    :: Ptr (Band s t a) -> CInt -> CInt -> Ptr a -> IO CInt

foldl'
  :: forall s t a b. GDALType a
  => (b -> Value a -> b) -> b -> Band s t a -> GDAL s b
foldl' f = foldlM' (\acc -> return . f acc)
{-# INLINE foldl' #-}

ifoldl'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> b) -> b -> Band s t a -> GDAL s b
ifoldl' f = ifoldlM' (\acc ix -> return . f acc ix)
{-# INLINE ifoldl' #-}

foldlM'
  :: forall s t a b. GDALType a
  => (b -> Value a -> IO b) -> b -> Band s t a -> GDAL s b
foldlM' f = ifoldlM' (\acc _ -> f acc)
{-# INLINE foldlM' #-}

ifoldlM'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> IO b) -> b -> Band s t a  -> GDAL s b
ifoldlM' f initialAcc band = liftIO $ do
  mNodata <- c_bandNodataValue (unBand band)
  fp <- mallocForeignPtrArray (sx*sy)
  withForeignPtr fp $ \ptr -> do
    let toValue = case mNodata of
                    Nothing -> Value
                    Just nd ->
                      \v -> if toNodata v == nd then NoData else Value v
        goB !iB !jB !acc
          | iB < nx   = do
              withLockedBandPtr band $ \b ->
                throwIfError_ "ifoldlM'" $
                  readBlock_ b (fromIntegral iB) (fromIntegral jB) ptr
              go 0 0 acc >>= goB (iB+1) jB
          | jB+1 < ny = goB 0 (jB+1) acc
          | otherwise = return acc
          where
            applyTo i j a = f a ix . toValue =<< peekElemOff ptr (j*sx+i)
              where ix = XY (iB*sx+i) (jB*sy+j)
            stopx
              | mx /= 0 && iB==nx-1 = mx
              | otherwise           = sx
            stopy
              | my /= 0 && jB==ny-1 = my
              | otherwise           = sy
            go !i !j !acc'
              | i   < stopx = applyTo i j acc' >>= go (i+1) j
              | j+1 < stopy = go 0 (j+1) acc'
              | otherwise   = return acc'
    goB 0 0 initialAcc
  where
    !(XY mx my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(XY nx ny) = bandBlockCount band
    !(XY sx sy) = bandBlockSize band
{-# INLINE ifoldlM' #-}

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> BlockIx  -> Vector (Value a) -> GDAL s ()
writeBandBlock band (XY x y) uvec = do
  checkType band
  liftIO $ withLockedBandPtr band $ \b -> do
    bNodata <- fmap (maybe nodata fromNodata) (c_bandNodataValue b)
    let (fp, len) = St.unsafeToForeignPtr0 vec
        vec       = St.map (fromValue bNodata) (uToStValue uvec)
        nElems    = bandBlockLen band
    if nElems /= len
      then throwBindingException (InvalidBlockSize len)
      else withForeignPtr fp $ \ptr ->
           throwIfError_ "writeBandBlock" $
              writeBlock_ b (fromIntegral x) (fromIntegral y) ptr
{-# INLINE writeBandBlock #-}

foreign import ccall safe "gdal.h GDALWriteBlock" writeBlock_
   :: Ptr (RWBand s a) -> CInt -> CInt -> Ptr a -> IO CInt

foreign import ccall safe "gdal.h GDALGetMaskBand" c_getMaskBand
   :: Ptr (Band s t a) -> IO (Ptr (Band s t Word8))

{#enum define MaskFlag { GMF_ALL_VALID   as MaskAllValid
                       , GMF_PER_DATASET as MaskPerDataset
                       , GMF_ALPHA       as MaskAlpha
                       , GMF_NODATA      as MaskNoData
                       } deriving (Eq,Bounded,Show) #}

foreign import ccall unsafe "gdal.h GDALGetMaskFlags" c_getMaskFlags
   :: Ptr (Band s t a) -> IO CInt


instance GDALType Word8 where
  datatype _ = GDT_Byte
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = truncate
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Word16 where
  datatype _ = GDT_UInt16
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = truncate
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Word32 where
  datatype _ = GDT_UInt32
  nodata = maxBound
  toNodata = fromIntegral
  fromNodata = truncate
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Int16 where
  datatype _ = GDT_Int16
  nodata = minBound
  toNodata = fromIntegral
  fromNodata = truncate
  {-# INLINE toNodata #-}
  {-# INLINE fromNodata #-}

instance GDALType Int32 where
  datatype _ = GDT_Int32
  nodata = minBound
  toNodata = fromIntegral
  fromNodata = truncate
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

#ifdef STORABLE_COMPLEX
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
#endif
