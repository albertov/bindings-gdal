{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

#include "bindings.h"

module GDAL.Internal.GDAL (
    GDALType (..)
  , GDALRasterException (..)
  , DataType (..)
  , Geotransform (..)
  , OverviewResampling (..)
  , Driver (..)
  , Dataset
  , DatasetH (..)
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , RasterBandH (..)

  , northUpGeotransform
  , gcpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)

  , nullDatasetH
  , allRegister
  , destroyDriverManager
  , create
  , createMem
  , delete
  , rename
  , copyFiles
  , flushCache
  , closeDataset
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy
  , buildOverviews
  , driverCreationOptionList

  , dataTypeSize
  , dataTypeByName
  , dataTypeUnion
  , dataTypeIsComplex
  , bandTypedAs
  , bandCoercedTo
  {-
  , reifyBandDataType
  , reifyDataType
  -}

  , datasetDriver
  , datasetSize
  , datasetFileList
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetGCPs
  , setDatasetGCPs
  , datasetBandCount

  , bandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandSize
  , bandHasOverviews
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , addBand
  , readBand
  , readBandBlock
  , writeBand
  , writeBandBlock
  , copyBand

  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'

  , unDataset
  , unBand
  , version
  , newDatasetHandle
  , openDatasetCount
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.Exception (Exception(..))
import Control.Monad (liftM, liftM2, when, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Int (Int16, Int32)
import Data.Bits ((.&.))
import Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Complex (Complex(..), realPart)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import Foreign.C.String (withCString, CString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArrayLen)
import Foreign.Marshal.Utils (toBool, fromBool, with)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Types
import GDAL.Internal.Common
import GDAL.Internal.Util
{#import GDAL.Internal.GCP#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLProgress#}
import GDAL.Internal.OSR
import GDAL.Internal.OGRGeometry (Envelope(..), envelopeSize)


#include "gdal.h"

newtype Driver = Driver ByteString
  deriving (Eq, IsString)

instance Show Driver where
  show (Driver s) = show s

version :: (Int, Int)
version = ({#const GDAL_VERSION_MAJOR#} , {#const GDAL_VERSION_MINOR#})

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDataType   !DataType
  | InvalidDriverOptions
  | UnknownRasterDataType
  | UnsupportedRasterDataType !DataType
  | NullDataset
  | NullBand
  | UnknownDriver !ByteString
  deriving (Typeable, Show, Eq)

instance Exception GDALRasterException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException


class (U.Unbox a, Storable a) => GDALType a where
  dataType :: Proxy a -> DataType
  -- | default nodata value when writing to bands with no datavalue set
  defaultNoData   :: a
  -- | how to convert to double for use in setBandNodataValue
  toCDouble :: a -> CDouble
  -- | how to convert from double for use with bandNodataValue
  fromCDouble :: CDouble -> a

  fillBand :: a -> RWBand s a -> GDAL s ()
  fillBand v = fillRaster (toCDouble v) (toCDouble v)
  {-# INLINE fillBand #-}

  mkToValue :: Maybe a -> (a -> Value a)
  mkToValue Nothing   v = Value v
  mkToValue (Just nd) v
    | vd == ndd  = NoData
    | otherwise  = Value v
    where
      vd = toCDouble v
      ndd = toCDouble nd
  {-# INLINE mkToValue #-}

  mkFromValue :: Maybe a -> (Value a -> a)
  mkFromValue = fromValue . fromMaybe defaultNoData
  {-# INLINE mkFromValue #-}

{#enum DataType {} omit (GDT_TypeCount) deriving (Eq, Show, Bounded) #}

{#fun pure unsafe GetDataTypeSize as dataTypeSize
    { fromEnumC `DataType' } -> `Int' #}

{#fun pure unsafe DataTypeIsComplex as ^
    { fromEnumC `DataType' } -> `Bool' #}

{#fun pure unsafe GetDataTypeByName as dataTypeByName
    { `String' } -> `DataType' toEnumC #}

{#fun pure unsafe DataTypeUnion as ^
    { fromEnumC `DataType', fromEnumC `DataType' } -> `DataType' toEnumC #}


{#enum GDALAccess {} deriving (Eq, Show) #}

{#enum RWFlag {} deriving (Eq, Show) #}

{#pointer MajorObjectH newtype#}

class MajorObject o (t::AccessMode) where
  majorObject     :: o t -> MajorObjectH


{#pointer DatasetH newtype #}


nullDatasetH :: DatasetH
nullDatasetH = DatasetH nullPtr

deriving instance Eq DatasetH

newtype Dataset s (t::AccessMode) =
  Dataset (ReleaseKey, DatasetH)

instance MajorObject (Dataset s) t where
  majorObject ds =
    let DatasetH p = unDataset ds
    in MajorObjectH (castPtr p)

unDataset :: Dataset s t -> DatasetH
unDataset (Dataset (_,s)) = s

closeDataset :: MonadIO m => Dataset s t -> m ()
closeDataset (Dataset (rk,_)) = release rk

type RODataset s = Dataset s ReadOnly
type RWDataset s = Dataset s ReadWrite

{#pointer RasterBandH newtype #}

nullBandH :: RasterBandH
nullBandH = RasterBandH nullPtr

deriving instance Eq RasterBandH

newtype Band s a (t::AccessMode) =
  Band { unBand :: RasterBandH }

bandTypedAs :: Band s a t -> a -> Band s a t
bandTypedAs = const . id

bandCoercedTo :: Band s a t -> b -> Band s b t
bandCoercedTo = const . coerce

instance MajorObject (Band s a) t where
  majorObject b =
    let RasterBandH p = unBand b
    in MajorObjectH (castPtr p)

{-
reifyBandDataType
  :: Band s t -> (forall a. GDALType a => Proxy a -> b) -> b
reifyBandDataType b = reifyDataType (bandDataType b)
-}

type ROBand s a = Band s a ReadOnly
type RWBand s a = Band s a ReadWrite


{#pointer DriverH #}

{#fun AllRegister as ^ {} -> `()'  #}

{#fun DestroyDriverManager as ^ {} -> `()'#}

driverByName :: Driver -> IO DriverH
driverByName (Driver s) = do
  d <- useAsCString s {#call unsafe GetDriverByName as ^#}
  if d == nullPtr
    then throwBindingException (UnknownDriver s)
    else return d

driverCreationOptionList :: Driver -> ByteString
driverCreationOptionList driver = unsafePerformIO $ do
  d <- driverByName driver
  {#call GetDriverCreationOptionList	as ^#} d >>= packCString

validateCreationOptions :: DriverH -> Ptr CString -> IO ()
validateCreationOptions d o = do
  valid <- liftM toBool ({#call GDALValidateCreationOptions as ^ #} d o)
  when (not valid) (throwBindingException InvalidDriverOptions)

create
  :: Driver -> String -> Size -> Int -> DataType -> OptionList
  -> GDAL s (Dataset s ReadWrite)
create _   _    _          _     GDT_Unknown _       =
  throwBindingException UnknownRasterDataType
create drv path (XY nx ny) bands dtype  options =
  newDatasetHandle $ withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC dtype
    d <- driverByName drv
    withOptionList options $ \opts -> do
      validateCreationOptions d opts
      {#call GDALCreate as ^#} d path' nx' ny' bands' dtype' opts

delete :: Driver -> String -> GDAL s ()
delete driver path =
  liftIO $
  checkCPLError "deleteDataset" $ do
    d <- driverByName driver
    withCString path ({#call DeleteDataset as ^#} d)

rename :: Driver -> String -> String -> GDAL s ()
rename driver newName oldName =
  liftIO $
  checkCPLError "renameDataset" $ do
    d <- driverByName driver
    withCString newName $
      withCString oldName . ({#call RenameDataset as ^#} d)

copyFiles :: Driver -> String -> String -> GDAL s ()
copyFiles driver newName oldName =
  liftIO $
  checkCPLError "copyFiles" $ do
    d <- driverByName driver
    withCString newName $
      withCString oldName . ({#call CopyDatasetFiles as ^#} d)

openReadOnly :: String -> GDAL s (RODataset s)
openReadOnly p = openWithMode GA_ReadOnly p

openReadWrite :: String -> GDAL s (RWDataset s)
openReadWrite p = openWithMode GA_Update p

openWithMode :: GDALAccess -> String -> GDAL s (Dataset s t)
openWithMode m path =
  newDatasetHandle $
  withCString path $
  flip {#call GDALOpen as ^#} (fromEnumC m)

unsafeToReadOnly :: RWDataset s -> GDAL s (RODataset s)
unsafeToReadOnly ds = flushCache ds >> return (coerce ds)

createCopy
  :: Driver -> String -> Dataset s t -> Bool -> OptionList
  -> Maybe ProgressFun -> GDAL s (RWDataset s)
createCopy driver path ds strict options progressFun =
  newDatasetHandle $
  withProgressFun "createCopy" progressFun $ \pFunc -> do
    d <- driverByName driver
    withOptionList options $ \o -> do
      validateCreationOptions d o
      withCString path $ \p ->
        {#call GDALCreateCopy as ^#}
          d p (unDataset ds) (fromBool strict) o pFunc nullPtr



newDatasetHandle :: IO DatasetH -> GDAL s (Dataset s t)
newDatasetHandle act =
  liftM Dataset $ allocate (checkGDALCall checkit act) free
  where
    checkit exc p
      | p==nullDatasetH = Just (fromMaybe
                                (GDALBindingException NullDataset) exc)
      | otherwise       = Nothing
    free ds = do
      refCount <- {#call unsafe DereferenceDataset as ^#} ds
      when (refCount<=0) ({#call GDALClose as ^#} ds)

createMem
  :: Size -> Int -> DataType -> OptionList -> GDAL s (Dataset s ReadWrite)
createMem = create "Mem" ""

flushCache :: forall s. RWDataset s -> GDAL s ()
flushCache = liftIO . {#call GDALFlushCache as ^#} . unDataset

datasetDriver :: Dataset s t -> Driver
datasetDriver ds =
  unsafePerformIO $
  liftM Driver $ do
    driver <-{#call unsafe GetDatasetDriver as ^#} (unDataset ds)
    {#call unsafe GetDriverShortName as ^#} driver >>= packCString

datasetSize :: Dataset s t -> Size
datasetSize ds =
  fmap fromIntegral $
     XY ({#call pure unsafe GetRasterXSize as ^#} (unDataset ds))
        ({#call pure unsafe GetRasterYSize as ^#} (unDataset ds))

datasetFileList :: Dataset s t -> GDAL s [Text]
datasetFileList =
  liftIO .
  fromCPLStringList .
  {#call unsafe GetFileList as ^#} .
  unDataset

datasetProjection :: Dataset s t -> GDAL s (Maybe SpatialReference)
datasetProjection =
  liftIO . ({#call unsafe GetProjectionRef as ^#} . unDataset
              >=> maybeSpatialReferenceFromCString)


setDatasetProjection :: RWDataset s -> SpatialReference -> GDAL s ()
setDatasetProjection ds srs =
  liftIO $ checkCPLError "SetProjection" $
    unsafeUseAsCString (srsToWkt srs)
      ({#call unsafe SetProjection as ^#} (unDataset ds))

setDatasetGCPs
  :: RWDataset s -> [GroundControlPoint] -> Maybe SpatialReference -> GDAL s ()
setDatasetGCPs ds gcps mSrs =
  liftIO $
  checkCPLError "setDatasetGCPs" $
  withGCPArrayLen gcps $ \nGcps pGcps ->
  withMaybeSRAsCString mSrs $
  {#call unsafe GDALSetGCPs as ^#}
    (unDataset ds)
    (fromIntegral nGcps)
    pGcps

datasetGCPs
  :: Dataset s t
  -> GDAL s ([GroundControlPoint], Maybe SpatialReference)
datasetGCPs ds =
  liftIO $ do
    let pDs = unDataset ds
    srs <- maybeSpatialReferenceFromCString
            =<< {#call unsafe GetGCPProjection as ^#} pDs
    nGcps <- liftM fromIntegral ({#call unsafe GetGCPCount as ^#} pDs)
    gcps <- {#call unsafe GetGCPs as ^#} pDs >>= fromGCPArray nGcps
    return (gcps, srs)


data OverviewResampling
  = OvNearest
  | OvGauss
  | OvCubic
  | OvAverage
  | OvMode
  | OvAverageMagphase
  | OvNone

buildOverviews
  :: RWDataset s -> OverviewResampling -> [Int] -> [Int] -> Maybe ProgressFun
  -> GDAL s ()
buildOverviews ds resampling overviews bands progressFun =
  liftIO $
  withResampling resampling $ \pResampling ->
  withArrayLen (map fromIntegral overviews) $ \nOverviews pOverviews ->
  withArrayLen (map fromIntegral bands) $ \nBands pBands ->
  withProgressFun "buildOverviews" progressFun $ \pFunc ->
  checkCPLError "buildOverviews" $
    {#call GDALBuildOverviews as ^#}
      (unDataset ds)
      pResampling
      (fromIntegral nOverviews)
      pOverviews
      (fromIntegral nBands)
      pBands
      pFunc
      nullPtr
  where
    withResampling OvNearest         = unsafeUseAsCString "NEAREST\0"
    withResampling OvGauss           = unsafeUseAsCString "GAUSS\0"
    withResampling OvCubic           = unsafeUseAsCString "CUBIC\0"
    withResampling OvAverage         = unsafeUseAsCString "AVERAGE\0"
    withResampling OvMode            = unsafeUseAsCString "MODE\0"
    withResampling OvAverageMagphase = unsafeUseAsCString "AVERAGE_MAGPHASE\0"
    withResampling OvNone            = unsafeUseAsCString "NONE\0"

data Geotransform
  = Geotransform {
      gtXOff   :: {-# UNPACK #-} !Double
    , gtXDelta :: {-# UNPACK #-} !Double
    , gtXRot   :: {-# UNPACK #-} !Double
    , gtYOff   :: {-# UNPACK #-} !Double
    , gtYRot   :: {-# UNPACK #-} !Double
    , gtYDelta :: {-# UNPACK #-} !Double
  } deriving (Eq, Show)

northUpGeotransform :: Size -> Envelope Double -> Geotransform
northUpGeotransform size envelope =
  Geotransform {
      gtXOff   = px (envelopeMin envelope)
    , gtXDelta = px (envelopeSize envelope / fmap fromIntegral size)
    , gtXRot   = 0
    , gtYOff   = py (envelopeMax envelope)
    , gtYRot   = 0
    , gtYDelta = negate (py (envelopeSize envelope / fmap fromIntegral size))
  }

gcpGeotransform :: [GroundControlPoint] -> ApproxOK -> Maybe Geotransform
gcpGeotransform gcps approxOk =
  unsafePerformIO $
  alloca $ \pGt ->
  withGCPArrayLen gcps $ \nGcps pGcps -> do
    ret <- liftM toBool $
           {#call unsafe GCPsToGeoTransform as ^#}
             (fromIntegral nGcps)
             pGcps
             (castPtr pGt)
             (fromEnumC approxOk)
    if ret then liftM Just (peek pGt) else return Nothing

applyGeotransform :: Geotransform -> XY Double -> XY Double
applyGeotransform Geotransform{..} (XY x y) =
  XY (gtXOff + gtXDelta*x + gtXRot   * y)
     (gtYOff + gtYRot  *x + gtYDelta * y)
{-# INLINE applyGeotransform #-}

infixr 5 |$|
(|$|) :: Geotransform -> XY Double -> XY Double
(|$|) = applyGeotransform

invertGeotransform :: Geotransform -> Maybe Geotransform
invertGeotransform (Geotransform g0 g1 g2 g3 g4 g5)
  | g2==0 && g4==0 && g1/=0 && g5/=0 -- No rotation
  = Just $! Geotransform (-g0/g1) (1/g1) 0 (-g3/g5) 0 (1/g5)
  | abs det < 1e-15 = Nothing -- not invertible
  | otherwise
  = Just $! Geotransform ((g2*g3 - g0*g5) * idet)
                         (g5*idet)
                         (-g2*idet)
                         ((-g1*g3 + g0*g4) * idet)
                         (-g4*idet)
                         (g1*idet)
  where
    idet = 1/det
    det  = g1*g5 - g2*g4
{-# INLINE invertGeotransform #-}

inv :: Geotransform -> Geotransform
inv = fromMaybe (error "Could not invert geotransform") . invertGeotransform

-- | Creates a Geotransform equivalent to applying a and then b
composeGeotransforms :: Geotransform -> Geotransform -> Geotransform
b `composeGeotransforms` a =
  Geotransform (b1*a0 + b2*a3 + b0)
               (b1*a1 + b2*a4)
               (b1*a2 + b2*a5)
               (b4*a0 + b5*a3 + b3)
               (b4*a1 + b5*a4)
               (b4*a2 + b5*a5)

  where
    Geotransform a0 a1 a2 a3 a4 a5 = a
    Geotransform b0 b1 b2 b3 b4 b5 = b
{-# INLINE composeGeotransforms #-}

infixr 9 |.|
(|.|) :: Geotransform -> Geotransform -> Geotransform
(|.|) = composeGeotransforms

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


datasetGeotransform :: Dataset s t -> GDAL s (Maybe Geotransform)
datasetGeotransform ds = liftIO $ alloca $ \p -> do
  ret <- {#call unsafe GetGeoTransform as ^#} (unDataset ds) (castPtr p)
  if toEnumC ret == CE_None
    then liftM Just (peek p)
    else return Nothing


setDatasetGeotransform :: RWDataset s -> Geotransform -> GDAL s ()
setDatasetGeotransform ds gt = liftIO $
  checkCPLError "SetGeoTransform" $
    with gt ({#call unsafe SetGeoTransform as ^#} (unDataset ds) . castPtr)


datasetBandCount :: Dataset s t -> GDAL s Int
datasetBandCount =
  liftM fromIntegral . liftIO . {#call unsafe GetRasterCount as ^#} . unDataset

getBand :: Int -> Dataset s t -> GDAL s (Band s a t)
getBand b ds =
  liftIO $
  liftM Band $
  checkGDALCall checkit $
  {#call GetRasterBand as ^#} (unDataset ds) (fromIntegral b)
  where
    checkit exc p
      | p == nullBandH = Just (fromMaybe (GDALBindingException NullBand) exc)
      | otherwise      = Nothing

addBand
  :: forall s a. GDALType a
  => RWDataset s -> OptionList -> GDAL s (RWBand s a)
addBand ds options = do
  liftIO $
    checkCPLError "addBand" $
    withOptionList options $
    {#call GDALAddBand as ^#} (unDataset ds) (fromEnumC dt)
  ix <- datasetBandCount ds
  getBand ix ds
  where dt = dataType (Proxy :: Proxy a)

{-
reifyDataType :: DataType -> (forall a. GDALType a => Proxy a -> b) -> b
reifyDataType dt f =
  case dt of
    GDT_Unknown  -> throw (bindingExceptionToException UnknownRasterDataType)
    GDT_Byte     -> f (Proxy :: Proxy Word8)
    GDT_UInt16   -> f (Proxy :: Proxy Word16)
    GDT_UInt32   -> f (Proxy :: Proxy Word32)
    GDT_Int16    -> f (Proxy :: Proxy Int16)
    GDT_Int32    -> f (Proxy :: Proxy Int32)
    GDT_Float32  -> f (Proxy :: Proxy Float)
    GDT_Float64  -> f (Proxy :: Proxy Double)
#ifdef STORABLE_COMPLEX
    GDT_CInt16   -> f (Proxy :: Proxy (Complex Int16))
    GDT_CInt32   -> f (Proxy :: Proxy (Complex Int32))
    GDT_CFloat32 -> f (Proxy :: Proxy (Complex Float))
    GDT_CFloat64 -> f (Proxy :: Proxy (Complex Double))
#else
    d            -> throw
                    (bindingExceptionToException (UnsupportedRasterDataType d))
#endif

-}



bandDataType :: Band s a t -> DataType
bandDataType = toEnumC . {#call pure unsafe GetRasterDataType as ^#} . unBand

bandBlockSize :: (Band s a t) -> Size
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   {#call unsafe GetBlockSize as ^#} (unBand band) xPtr yPtr
   liftM (fmap fromIntegral) (liftM2 XY (peek xPtr) (peek yPtr))

bandBlockLen :: Band s a t -> Int
bandBlockLen = (\(XY x y) -> x*y) . bandBlockSize

bandSize :: Band s a t -> Size
bandSize band =
  fmap fromIntegral $
    XY ({#call pure unsafe GetRasterBandXSize as ^#} (unBand band))
       ({#call pure unsafe GetRasterBandYSize as ^#} (unBand band))

allBand :: Band s a t -> Envelope Int
allBand = Envelope (pure 0) . bandSize

bandBlockCount :: Band s a t -> XY Int
bandBlockCount b = fmap ceiling $ liftA2 ((/) :: Double -> Double -> Double)
                     (fmap fromIntegral (bandSize b))
                     (fmap fromIntegral (bandBlockSize b))

bandHasOverviews :: Band s a t -> GDAL s Bool
bandHasOverviews =
  liftIO . liftM toBool . {#call unsafe HasArbitraryOverviews as ^#} . unBand

checkType
  :: forall s a t. GDALType a => Band s a t -> GDAL s ()
checkType b
  | rt == bt  = return ()
  | otherwise = throwBindingException (InvalidDataType bt)
  where rt = dataType (Proxy :: Proxy a)
        bt = bandDataType b

bandNodataValue :: GDALType a => Band s a t -> GDAL s (Maybe a)
bandNodataValue b =
  liftIO $
  alloca $ \p -> do
    value <- {#call unsafe GetRasterNoDataValue as ^#} (unBand b) p
    hasNodata <- liftM toBool $ peek p
    return (if hasNodata then Just (fromCDouble value) else Nothing)
{-# INLINE bandNodataValue #-}


setBandNodataValue :: GDALType a => RWBand s a -> a -> GDAL s ()
setBandNodataValue b v =
  liftIO $
  checkCPLError "SetRasterNoDataValue" $
  {#call unsafe SetRasterNoDataValue as ^#} (unBand b) (toCDouble v)

fillRaster :: CDouble -> CDouble  -> RWBand s a -> GDAL s ()
fillRaster r i b =
  liftIO $
    checkCPLError "FillRaster" $
      {#call GDALFillRaster as ^#} (unBand b) r i


readBand :: forall s t a. GDALType a
  => (Band s a t)
  -> Envelope Int
  -> Size
  -> GDAL s (Vector (Value a))
readBand band win (XY bx by) = readMasked band read_
  where
    XY sx sy     = envelopeSize win
    XY xoff yoff = envelopeMin win
    read_ :: forall a'. GDALType a' => RasterBandH -> IO (St.Vector a')
    read_ b = do
      vec <- Stm.new (bx*by)
      let dtype = fromEnumC (dataType (Proxy :: Proxy a'))
      Stm.unsafeWith vec $ \ptr -> do
        checkCPLError "RasterAdviseRead" $
          {#call unsafe RasterAdviseRead as ^#}
            b
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            (fromIntegral bx)
            (fromIntegral by)
            dtype
            (castPtr nullPtr)
        checkCPLError "RasterIO" $
          {#call RasterIO as ^#}
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
      St.unsafeFreeze vec
{-# INLINE readBand #-}

readMasked
  :: GDALType a
  => Band s a t
  -> (forall a'. GDALType a' => RasterBandH -> IO (St.Vector a'))
  -> GDAL s (Vector (Value a))
readMasked band reader = do
  vec   <- liftIO (reader bPtr)
  flags <- liftIO ({#call unsafe GetMaskFlags as ^#} bPtr)
  liftM U.convert (mask flags vec)
  where
    bPtr = unBand band
    mask fs
      | hasFlag fs MaskPerDataset = useMaskBand
      | hasFlag fs MaskNoData     = useNoData
      | hasFlag fs MaskAllValid   = useAsIs
      | otherwise                 = useMaskBand
    hasFlag fs f = fromEnumC f .&. fs == fromEnumC f
    useAsIs  = return . U.map Value . St.convert
    useNoData vs   = do
      toValue <- liftM mkToValue (bandNodataValue band)
      return $! U.map toValue (St.convert vs)
    useMaskBand vs = liftIO $ do
      ms <- {#call GetMaskBand as ^#} bPtr >>= reader :: IO (St.Vector Word8)
      return $ U.zipWith (\v m -> if m/=0 then Value v else NoData)
                         (U.convert vs)
                         (U.convert ms)
{-# INLINE readMasked #-}

{#enum define MaskFlag { GMF_ALL_VALID   as MaskAllValid
                       , GMF_PER_DATASET as MaskPerDataset
                       , GMF_ALPHA       as MaskAlpha
                       , GMF_NODATA      as MaskNoData
                       } deriving (Eq,Bounded,Show) #}


writeBand :: forall s a. GDALType a
  => RWBand s a
  -> Envelope Int
  -> Size
  -> Vector (Value a)
  -> GDAL s ()
writeBand band win sz@(XY bx by) uvec = do
  fromVal <- liftM mkFromValue (bandNodataValue band)
  let nElems    = bx * by
      (fp, len) = St.unsafeToForeignPtr0 vec
      vec          = St.convert (U.map fromVal uvec)
      XY sx sy     = envelopeSize win
      XY xoff yoff = envelopeMin win
  if nElems /= len
    then throwBindingException (InvalidRasterSize sz)
    else liftIO $
         withForeignPtr fp $ \ptr ->
         checkCPLError "RasterIO" $
           {#call RasterIO as ^#}
             (unBand band)
             (fromEnumC GF_Write)
             (fromIntegral xoff)
             (fromIntegral yoff)
             (fromIntegral sx)
             (fromIntegral sy)
             (castPtr ptr)
             (fromIntegral bx)
             (fromIntegral by)
             (fromEnumC (dataType (Proxy :: Proxy a)))
             0
             0
{-# INLINE writeBand #-}

readBandBlock
  :: forall s t a. GDALType a
  => Band s a t -> BlockIx -> GDAL s (Vector (Value a))
readBandBlock band blockIx = do
  checkType band
  readMasked band $ \b -> do
    vec <- Stm.new (bandBlockLen band)
    Stm.unsafeWith vec $
      checkCPLError "ReadBlock" . {#call ReadBlock as ^#} b x y . castPtr
    St.unsafeFreeze vec
  where XY x y = fmap fromIntegral blockIx
{-# INLINE readBandBlock #-}

copyBand
  :: Band s a t -> RWBand s a -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand src dst options progressFun =
  liftIO $
  withProgressFun "copyBand" progressFun $ \pFunc ->
  withOptionList options $ \o ->
  checkCPLError "copyBand" $
  {#call RasterBandCopyWholeRaster as ^#}
    (unBand src) (unBand dst) o pFunc nullPtr

foldl'
  :: forall s t a b. GDALType a
  => (b -> Value a -> b) -> b -> Band s a t -> GDAL s b
foldl' f = foldlM' (\acc -> return . f acc)
{-# INLINE foldl' #-}

ifoldl'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> b) -> b -> Band s a t -> GDAL s b
ifoldl' f = ifoldlM' (\acc ix -> return . f acc ix)
{-# INLINE ifoldl' #-}

foldlM'
  :: forall s t a b. GDALType a
  => (b -> Value a -> IO b) -> b -> Band s a t -> GDAL s b
foldlM' f = ifoldlM' (\acc _ -> f acc)
{-# INLINE foldlM' #-}

ifoldlM'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> IO b) -> b -> Band s a t -> GDAL s b
ifoldlM' f initialAcc band = do
  checkType band
  toValue <- liftM mkToValue (bandNodataValue band)
  liftIO $ do
    allocaArray (sx*sy) $ \ptr -> do
      let goB !iB !jB !acc
            | iB < nx   = do
                checkCPLError "ReadBlock" $
                  {#call ReadBlock as ^#}
                    pBand (fromIntegral iB) (fromIntegral jB) (castPtr ptr)
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
    pBand       = unBand band
    !(XY mx my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(XY nx ny) = bandBlockCount band
    !(XY sx sy) = bandBlockSize band
{-# INLINE ifoldlM' #-}

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> BlockIx  -> Vector (Value a) -> GDAL s ()
writeBandBlock band blockIx uvec = do
  checkType band
  fromVal <- liftM mkFromValue (bandNodataValue band)
  liftIO $ do
    let (fp, len) = St.unsafeToForeignPtr0 vec
        vec       = St.convert (U.map fromVal uvec)
        nElems    = bandBlockLen band
    if nElems /= len
      then throwBindingException (InvalidBlockSize len)
      else withForeignPtr fp $
           checkCPLError "WriteBlock" .
           {#call WriteBlock as ^#} pBand x y .
           castPtr
  where XY x y = fmap fromIntegral blockIx
        pBand  = unBand band
{-# INLINE writeBandBlock #-}

openDatasetCount :: IO Int
openDatasetCount =
  alloca $ \ppDs ->
  alloca $ \pCount -> do
    {#call unsafe GetOpenDatasets as ^#} ppDs pCount
    liftM fromIntegral (peek pCount)

metadataDomains :: MajorObject o t => o t -> GDAL s [Text]
#if SUPPORTS_METADATA_DOMAINS
metadataDomains o =
  liftIO $
  fromCPLStringList $
  {#call unsafe GetMetadataDomainList as ^#} (majorObject o)
#else
metadataDomains = const (return [])
#endif

metadata
  :: MajorObject o t
  => Maybe ByteString -> o t -> GDAL s [(Text,Text)]
metadata domain o =
  liftIO $
  withMaybeByteString domain
  ({#call unsafe GetMetadata as ^#} (majorObject o)
    >=> liftM (map breakIt) . fromBorrowedCPLStringList)
  where
    breakIt s =
      case T.break (=='=') s of
        (k,"") -> (k,"")
        (k, v) -> (k, T.tail v)

metadataItem
  :: MajorObject o t
  => Maybe ByteString -> ByteString -> o t -> GDAL s (Maybe ByteString)
metadataItem domain key o =
  liftIO $
  useAsCString key $ \pKey ->
  withMaybeByteString domain $
    {#call unsafe GetMetadataItem as ^#} (majorObject o) pKey >=> 
      maybePackCString

setMetadataItem
  :: (MajorObject o t, t ~ ReadWrite)
  => Maybe ByteString -> ByteString -> ByteString -> o t -> GDAL s ()
setMetadataItem domain key val o =
  liftIO $
  checkCPLError "setMetadataItem" $
  useAsCString key $ \pKey ->
  useAsCString val $ \pVal ->
  withMaybeByteString domain $
    {#call unsafe GDALSetMetadataItem as ^#} (majorObject o) pKey pVal

description :: MajorObject o t => o t -> GDAL s ByteString
description =
  liftIO . ({#call unsafe GetDescription as ^#} . majorObject >=> packCString)

setDescription
  :: (MajorObject o t, t ~ ReadWrite)
  => ByteString -> o t -> GDAL s ()
setDescription val =
  liftIO . checkGDALCall_ const .
  useAsCString val . {#call unsafe GDALSetDescription as ^#} . majorObject

instance GDALType Word8 where
  dataType _ = GDT_Byte
  defaultNoData = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  dataType _ = GDT_UInt16
  defaultNoData = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  dataType _ = GDT_UInt32
  defaultNoData = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  dataType _ = GDT_Int16
  defaultNoData = minBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  dataType _ = GDT_Int32
  defaultNoData = minBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  dataType _ = GDT_Float32
  defaultNoData = defaultFractionalNodata
  fromCDouble = realToFrac
  toCDouble = realToFrac
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  dataType _ = GDT_Float64
  defaultNoData = defaultFractionalNodata
  toCDouble = realToFrac
  fromCDouble = realToFrac
  mkToValue = mkToValueEq
  {-# INLINE mkToValue #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

#ifdef STORABLE_COMPLEX
instance GDALType (Complex Int16) where
  dataType _ = GDT_CInt16
  defaultNoData = defaultNoData :+ 0
  toCDouble = fromIntegral . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Int32) where
  dataType _ = GDT_CInt32
  defaultNoData = defaultNoData :+ 0
  toCDouble = fromIntegral . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Float) where
  dataType _ = GDT_CFloat32
  defaultNoData = defaultNoData :+ 0
  toCDouble = realToFrac . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Double) where
  dataType _ = GDT_CFloat64
  defaultNoData = defaultNoData :+ 0
  toCDouble = realToFrac . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}
#endif

mkToValueEq :: Eq a => Maybe a -> (a -> Value a)
mkToValueEq Nothing   v = Value v
mkToValueEq (Just nd) v
  | v == nd   = NoData
  | otherwise = Value v
{-# INLINE mkToValueEq #-}

defaultFractionalNodata :: Fractional a => a
defaultFractionalNodata = (-1)/0
{-# INLINE defaultFractionalNodata #-}
