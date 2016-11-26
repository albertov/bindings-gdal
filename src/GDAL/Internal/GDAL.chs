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
{-# LANGUAGE LambdaCase #-}

#include "bindings.h"

module GDAL.Internal.GDAL (
    GDALRasterException (..)
  , Geotransform (..)
  , OverviewResampling (..)
  , DriverName (..)
  , Driver (..)
  , DriverH (..)
  , Dataset
  , DatasetH (..)
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , RasterBandH (..)
  , MaskType (..)

  , northUpGeotransform
  , gcpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)
  , geoEnvelopeTransformer

  , driverByName

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
  , openDatasetH
  , createDatasetH
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy
  , buildOverviews
  , driverCreationOptionList

  , bandAs

  , datasetDriver
  , datasetDriverName
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
  , bandProjection
  , bandGeotransform
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
  , fillBand
  , fmapBand
  , foldBands
  , readBand
  , createBandMask
  , bandMask
  , readBandBlock
  , writeBand
  , writeBandBlock
  , copyBand
  , bandConduit
  , unsafeBandConduit
  , bandSink
  , bandSinkGeo

  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , foldl'
  , ifoldl'
  , foldlWindow'
  , ifoldlWindow'
  , blockConduit
  , unsafeBlockConduit
  , blockSource
  , unsafeBlockSource
  , blockSink
  , allBlocks
  , zipBlocks
  , getZipBlocks

  , unDataset
  , unBand
  , version
  , newDatasetHandle
  , openDatasetCount

  , module GDAL.Internal.DataType
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Arrow (second)
import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Exception (Exception(..))
import Control.DeepSeq (NFData(..))
import Control.Monad (liftM, liftM2, when, (>=>), (<=<), forever)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Bits ((.&.))
import Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Conduit.Internal (Pipe(..), ConduitM(..), injectLeftovers)
import Data.Maybe (fromMaybe, fromJust)
import Data.String (IsString)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Storable         as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector.Generic.Mutable  as M
import qualified Data.Vector.Unboxed.Mutable  as UM
import qualified Data.Vector.Unboxed          as U
import Data.Word (Word8)

import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (toBool, fromBool, with)

import GHC.Types (SPEC(..))

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Types
import GDAL.Internal.Types.Value
import GDAL.Internal.DataType
import GDAL.Internal.Common
import GDAL.Internal.Util
{#import GDAL.Internal.GCP#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLProgress#}
import GDAL.Internal.OSR
import GDAL.Internal.OGRGeometry (Envelope(..), envelopeSize)


#include "gdal.h"

newtype DriverName = DriverName ByteString
  deriving (Eq, IsString)

instance Show DriverName where
  show (DriverName s) = show s

version :: (Int, Int)
version = ({#const GDAL_VERSION_MAJOR#} , {#const GDAL_VERSION_MINOR#})

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDriverOptions
  | UnknownRasterDataType
  | NullDataset
  | NullBand
  | UnknownDriver !ByteString
  | BandDoesNotAllowNoData
  | CannotInvertGeotransform
  | NotImplemented !ByteString
  deriving (Typeable, Show, Eq)

instance Exception GDALRasterException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException


{#enum GDALAccess {} deriving (Eq, Show) #}

{#enum RWFlag {} deriving (Eq, Show) #}

{#pointer MajorObjectH newtype#}

class MajorObject o (t::AccessMode) where
  majorObject     :: o t -> MajorObjectH


{#pointer DatasetH newtype #}


nullDatasetH :: DatasetH
nullDatasetH = DatasetH nullPtr

deriving instance Eq DatasetH
deriving instance NFData DatasetH

newtype Dataset s a (t::AccessMode) =
  Dataset (Maybe ReleaseKey, DatasetH)

instance MajorObject (Dataset s a) t where
  majorObject ds =
    let DatasetH p = unDataset ds
    in MajorObjectH (castPtr p)

unDataset :: Dataset s a t -> DatasetH
unDataset (Dataset (_,s)) = s

closeDataset :: MonadIO m => Dataset s a t -> m ()
closeDataset (Dataset (Just rk,_)) = release rk
closeDataset _                     = return ()

type RODataset s a = Dataset s a ReadOnly
type RWDataset s a = Dataset s a ReadWrite

{#pointer RasterBandH newtype #}

nullBandH :: RasterBandH
nullBandH = RasterBandH nullPtr

deriving instance Eq RasterBandH

newtype Band s a (t::AccessMode) = Band (RasterBandH, DataTypeK)

unBand :: Band s a t -> RasterBandH
unBand (Band (p,_)) = p

bandDataType :: Band s a t -> DataTypeK
bandDataType (Band (_,t)) = t
{-# INLINE bandDataType #-}

bandAs :: Band s a t -> DataType d -> Band s (HsType d) t
bandAs = const . coerce
{-# INLINE bandAs #-}

instance MajorObject (Band s a) t where
  majorObject b =
    let RasterBandH p = unBand b
    in MajorObjectH (castPtr p)

type ROBand s a = Band s a ReadOnly
type RWBand s a = Band s a ReadWrite


{#pointer DriverH newtype #}

newtype Driver (t::AccessMode) = Driver { unDriver :: DriverH }

instance MajorObject Driver t where
  majorObject d =
    let DriverH p = unDriver d
    in MajorObjectH (castPtr p)

{#fun AllRegister as ^ {} -> `()'  #}

{#fun DestroyDriverManager as ^ {} -> `()'#}

driverByName :: MonadIO m => DriverName -> m (Driver t)
driverByName (DriverName s) = liftIO $ do
  d <- useAsCString s {#call unsafe GetDriverByName as ^#}
  let DriverH p = d in if p == nullPtr
    then throwBindingException (UnknownDriver s)
    else return (Driver d)

driverHByName :: MonadIO m => DriverName -> m DriverH
driverHByName s = (\(Driver h) -> h) <$> driverByName s

driverCreationOptionList :: DriverName -> ByteString
driverCreationOptionList driver = unsafePerformIO $ do
  d <- driverHByName driver
  {#call GetDriverCreationOptionList	as ^#} d >>= packCString

validateCreationOptions :: DriverH -> Ptr CString -> IO ()
validateCreationOptions d o = do
  valid <- liftM toBool ({#call GDALValidateCreationOptions as ^ #} d o)
  when (not valid) (throwBindingException InvalidDriverOptions)

create
  :: DriverName -> String -> Size -> Int -> DataType d -> OptionList
  -> GDAL s (Dataset s (HsType d) ReadWrite)
create drv path size bands dtype optList = do
  d <- driverByName drv
  newDatasetHandle $ createDatasetH  d path size bands dtype optList

createDatasetH
  :: MonadIO m
  => Driver t -> String -> Size -> Int -> DataType d -> OptionList
  -> m DatasetH
createDatasetH drv path (nx :+: ny) bands dtype  opts = liftIO $
  withCString path $ \path' -> do
    let nx'    = fromIntegral nx
        ny'    = fromIntegral ny
        bands' = fromIntegral bands
        dtype' = fromEnumC dtype
        d      = unDriver drv
    withOptionList opts $ \o -> do
      validateCreationOptions d o
      {#call GDALCreate as ^#} d path' nx' ny' bands' dtype' o

delete :: DriverName -> String -> GDAL s ()
delete driver path =
  liftIO $
  checkCPLError "deleteDataset" $ do
    d <- driverHByName driver
    withCString path ({#call DeleteDataset as ^#} d)

rename :: DriverName -> String -> String -> GDAL s ()
rename driver newName oldName =
  liftIO $
  checkCPLError "renameDataset" $ do
    d <- driverHByName driver
    withCString newName $
      withCString oldName . ({#call RenameDataset as ^#} d)

copyFiles :: DriverName -> String -> String -> GDAL s ()
copyFiles driver newName oldName =
  liftIO $
  checkCPLError "copyFiles" $ do
    d <- driverHByName driver
    withCString newName $
      withCString oldName . ({#call CopyDatasetFiles as ^#} d)

openReadOnly :: String -> DataType d -> GDAL s (RODataset s (HsType d))
openReadOnly p _ = openWithMode GA_ReadOnly p

openReadWrite :: String -> DataType d -> GDAL s (RWDataset s (HsType d))
openReadWrite p _ = openWithMode GA_Update p

openWithMode :: GDALAccess -> String -> GDAL s (Dataset s a t)
openWithMode m = newDatasetHandle . openDatasetH m

openDatasetH :: MonadIO m => GDALAccess -> String -> m DatasetH
openDatasetH m path =
  liftIO $
  withCString path $
  flip {#call GDALOpen as ^#} (fromEnumC m)

unsafeToReadOnly :: RWDataset s a -> GDAL s (RODataset s a)
unsafeToReadOnly ds = flushCache ds >> return (coerce ds)

createCopy
  :: DriverName -> String -> Dataset s a t -> Bool -> OptionList
  -> Maybe ProgressFun -> GDAL s (RWDataset s a)
createCopy driver path ds strict opts progress =
  newDatasetHandle $
  withProgressFun "createCopy" progress $ \pFunc -> do
    d <- driverHByName driver
    withOptionList opts $ \o -> do
      validateCreationOptions d o
      withCString path $ \p ->
        {#call GDALCreateCopy as ^#}
          d p (unDataset ds) (fromBool strict) o pFunc nullPtr



newDatasetHandle :: IO DatasetH -> GDAL s (Dataset s a t)
newDatasetHandle act = do
  (rk,ds) <- allocate (checkGDALCall checkit act) free
  return (Dataset (Just rk, ds))
  where
    checkit exc p
      | p==nullDatasetH = Just (fromMaybe
                                (GDALBindingException NullDataset) exc)
      | otherwise       = Nothing
    free = {#call GDALClose as ^#}

createMem
  :: Size -> Int -> DataType d -> OptionList
  -> GDAL s (Dataset s (HsType d) ReadWrite)
createMem = create "Mem" ""

flushCache :: RWDataset s a -> GDAL s ()
flushCache = liftIO . {#call GDALFlushCache as ^#} . unDataset

datasetDriver :: Dataset s a t -> Driver t'
datasetDriver ds =
  unsafePerformIO $
  Driver <$> {#call unsafe GetDatasetDriver as ^#} (unDataset ds)

datasetDriverName :: Dataset s a t -> DriverName
datasetDriverName ds = unsafePerformIO $
  DriverName <$> (packCString =<< {#call unsafe GetDriverShortName as ^#} d)
  where Driver d = datasetDriver ds

datasetSize :: Dataset s a t -> Size
datasetSize ds =
  fmap fromIntegral $
        ({#call pure unsafe GetRasterXSize as ^#} (unDataset ds))
    :+: ({#call pure unsafe GetRasterYSize as ^#} (unDataset ds))

datasetFileList :: Dataset s a t -> GDAL s [Text]
datasetFileList =
  liftIO .
  fromCPLStringList .
  {#call unsafe GetFileList as ^#} .
  unDataset

datasetProjection :: MonadIO m => Dataset s a t -> m (Maybe SpatialReference)
datasetProjection
  = liftIO
  .  (maybeSpatialReferenceFromCString <=< {#call unsafe GetProjectionRef as ^#})
  . unDataset


setDatasetProjection :: SpatialReference -> RWDataset s a -> GDAL s ()
setDatasetProjection srs ds =
  liftIO $ checkCPLError "SetProjection" $
    useAsCString (srsToWkt srs)
      ({#call unsafe SetProjection as ^#} (unDataset ds))

setDatasetGCPs
  :: [GroundControlPoint] -> Maybe SpatialReference -> RWDataset s a
  -> GDAL s ()
setDatasetGCPs gcps mSrs ds =
  liftIO $
  checkCPLError "setDatasetGCPs" $
  withGCPArrayLen gcps $ \nGcps pGcps ->
  withMaybeSRAsCString mSrs $
  {#call unsafe GDALSetGCPs as ^#}
    (unDataset ds)
    (fromIntegral nGcps)
    pGcps

datasetGCPs
  :: Dataset s a t
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
  :: RWDataset s a -> OverviewResampling -> [Int] -> [Int] -> Maybe ProgressFun
  -> GDAL s ()
buildOverviews ds resampling overviews bands progress =
  liftIO $
  withResampling resampling $ \pResampling ->
  withArrayLen (map fromIntegral overviews) $ \nOverviews pOverviews ->
  withArrayLen (map fromIntegral bands) $ \nBands pBands ->
  withProgressFun "buildOverviews" progress $ \pFunc ->
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

instance NFData Geotransform where
  rnf Geotransform{} = ()

northUpGeotransform :: Size -> Envelope Double -> Geotransform
northUpGeotransform size envelope =
  Geotransform {
      gtXOff   = pFst (envelopeMin envelope)
    , gtXDelta = pFst (envelopeSize envelope / fmap fromIntegral size)
    , gtXRot   = 0
    , gtYOff   = pSnd (envelopeMax envelope)
    , gtYRot   = 0
    , gtYDelta = negate (pSnd (envelopeSize envelope / fmap fromIntegral size))
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

applyGeotransform :: Geotransform -> Pair Double -> Pair Double
applyGeotransform Geotransform{..} (x :+: y) =
  (gtXOff + gtXDelta*x + gtXRot * y) :+: (gtYOff + gtYRot  *x + gtYDelta * y)

infixr 5 |$|
(|$|) :: Geotransform -> Pair Double -> Pair Double
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


datasetGeotransform:: MonadIO m => Dataset s a t -> m (Maybe Geotransform)
datasetGeotransform ds = liftIO $ alloca $ \p -> do
  ret <- {#call unsafe GetGeoTransform as ^#} (unDataset ds) (castPtr p)
  if toEnumC ret == CE_None
    then liftM Just (peek p)
    else return Nothing

setDatasetGeotransform :: MonadIO m => Geotransform -> RWDataset s a -> m ()
setDatasetGeotransform gt ds = liftIO $
  checkCPLError "SetGeoTransform" $
    with gt ({#call unsafe SetGeoTransform as ^#} (unDataset ds) . castPtr)


datasetBandCount :: MonadIO m => Dataset s a t -> m Int
datasetBandCount =
  liftM fromIntegral . liftIO . {#call unsafe GetRasterCount as ^#} . unDataset

getBand :: Int -> Dataset s a t -> GDAL s (Band s a t)
getBand b ds = liftIO $ do
  pB <- checkGDALCall checkit $
         {#call GetRasterBand as ^#} (unDataset ds) (fromIntegral b)
  dt <- liftM toEnumC ({#call unsafe GetRasterDataType as ^#} pB)
  return (Band (pB, dt))
  where
    checkit exc p
      | p == nullBandH = Just (fromMaybe (GDALBindingException NullBand) exc)
      | otherwise      = Nothing

addBand
  :: forall s a. GDALType a
  => OptionList -> RWDataset s a -> GDAL s (RWBand s a)
addBand opts ds = do
  liftIO $
    checkCPLError "addBand" $
    withOptionList opts $
    {#call GDALAddBand as ^#} (unDataset ds) (fromEnumC dt)
  ix <- datasetBandCount ds
  getBand ix ds
  where dt = hsDataType (Proxy :: Proxy a)


bandBlockSize :: (Band s a t) -> Size
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   {#call unsafe GetBlockSize as ^#} (unBand band) xPtr yPtr
   liftM (fmap fromIntegral) (liftM2 (:+:) (peek xPtr) (peek yPtr))

bandBlockLen :: Band s a t -> Int
bandBlockLen = (\(x :+: y) -> x*y) . bandBlockSize

bandSize :: Band s a t -> Size
bandSize band =
  fmap fromIntegral $
        ({#call pure unsafe GetRasterBandXSize as ^#} (unBand band))
    :+: ({#call pure unsafe GetRasterBandYSize as ^#} (unBand band))

allBand :: Band s a t -> Envelope Int
allBand = Envelope (pure 0) . bandSize

bandBlockCount :: Band s a t -> Pair Int
bandBlockCount b = fmap ceiling $ liftA2 ((/) :: Double -> Double -> Double)
                     (fmap fromIntegral (bandSize b))
                     (fmap fromIntegral (bandBlockSize b))

bandHasOverviews :: Band s a t -> GDAL s Bool
bandHasOverviews =
  liftIO . liftM toBool . {#call unsafe HasArbitraryOverviews as ^#} . unBand

bandNodataValue :: GDALType a => Band s a t -> GDAL s (Maybe a)
bandNodataValue b =
  liftIO $
  alloca $ \p -> do
    value <- {#call unsafe GetRasterNoDataValue as ^#} (unBand b) p
    hasNodata <- liftM toBool $ peek p
    return (if hasNodata then Just (fromCDouble value) else Nothing)


setBandNodataValue :: GDALType a => a -> RWBand s a -> GDAL s ()
setBandNodataValue v b =
  liftIO $
  checkCPLError "SetRasterNoDataValue" $
  {#call unsafe SetRasterNoDataValue as ^#} (unBand b) (toCDouble v)

createBandMask :: MaskType -> RWBand s a -> GDAL s ()
createBandMask maskType band = liftIO $
  checkCPLError "createBandMask" $
  {#call CreateMaskBand as ^#} (unBand band) cflags
  where cflags = maskFlagsForType maskType

readBand :: forall s t a. GDALType a
  => (Band s a t)
  -> Envelope Int
  -> Size
  -> GDAL s (U.Vector (Value a))
readBand band win sz = liftM fromJust $ runConduit $
  yield win =$= unsafeBandConduit sz band =$= CL.head
{-# INLINEABLE readBand #-}

bandConduit
  :: forall s a t. GDALType a
  => Size
  -> Band s a t
  -> Conduit (Envelope Int) (GDAL s) (U.Vector (Value a))
bandConduit sz band =
  unsafeBandConduitM sz band =$= CL.mapM (liftIO . U.freeze)
{-# INLINEABLE bandConduit #-}

unsafeBandConduit
  :: forall s a t. GDALType a
  => Size
  -> Band s a t
  -> Conduit (Envelope Int) (GDAL s) (U.Vector (Value a))
unsafeBandConduit sz band =
  unsafeBandConduitM sz band =$= CL.mapM (liftIO . U.unsafeFreeze)
{-# INLINEABLE unsafeBandConduit #-}


unsafeBandConduitM
  :: forall s a t. GDALType a
  => Size
  -> Band s a t
  -> Conduit (Envelope Int) (GDAL s) (UM.IOVector (Value a))
unsafeBandConduitM (bx :+: by) band = do
  buf <- liftIO (M.new (bx*by))
  lift (bandMaskType band) >>= \case
    MaskNoData -> do
      nd <- lift (noDataOrFail band)
      let vec = mkValueUMVector nd buf
      awaitForever $ \win -> do
        liftIO (read_ band buf win)
        yield vec
    MaskAllValid -> do
      let vec = mkAllValidValueUMVector buf
      awaitForever $ \win -> do
        liftIO (read_ band buf win)
        yield vec
    _ -> do
      mBuf <- liftIO $ M.replicate (bx*by) 0
      mask <- lift $ bandMask band
      let vec = mkMaskedValueUMVector mBuf buf
      awaitForever $ \win -> do
        liftIO (read_ band buf win >> read_ mask mBuf win)
        yield vec
  where
    read_ :: forall a'. GDALType a'
          => Band s a' t -> Stm.IOVector a' -> Envelope Int -> IO ()
    read_ b vec win = do
      let sx   :+: sy   = envelopeSize win
          xoff :+: yoff = envelopeMin win
      Stm.unsafeWith vec $ \ptr -> do
        checkCPLError "RasterAdviseRead" $
          {#call unsafe RasterAdviseRead as ^#}
            (unBand b)
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            (fromIntegral bx)
            (fromIntegral by)
            (fromEnumC (hsDataType (Proxy :: Proxy a')))
            nullPtr
        checkCPLError "RasterIO" $
          {#call RasterIO as ^#}
            (unBand b)
            (fromEnumC GF_Read)
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            (castPtr ptr)
            (fromIntegral bx)
            (fromIntegral by)
            (fromEnumC (hsDataType (Proxy :: Proxy a')))
            0
            0
{-# INLINEABLE unsafeBandConduitM #-}

geoEnvelopeTransformer
  :: Geotransform -> Maybe (Envelope Double -> Envelope Int)
geoEnvelopeTransformer gt =
  case (gtXDelta gt < 0, gtYDelta gt<0, invertGeotransform gt) of
    (_,_,Nothing)          -> Nothing
    (False,False,Just iGt) -> Just $ \(Envelope e0 e1) ->
      let e0' = fmap round (applyGeotransform iGt e0)
          e1' = fmap round (applyGeotransform iGt e1)
      in Envelope e0' e1'
    (True,False,Just iGt) -> Just $ \(Envelope e0 e1) ->
      let x1 :+: y0 = fmap round (applyGeotransform iGt e0)
          x0 :+: y1 = fmap round (applyGeotransform iGt e1)
      in Envelope (x0 :+: y0) (x1 :+: y1)
    (False,True,Just iGt) -> Just $ \(Envelope e0 e1) ->
      let x0 :+: y1 = fmap round (applyGeotransform iGt e0)
          x1 :+: y0 = fmap round (applyGeotransform iGt e1)
      in Envelope (x0 :+: y0) (x1 :+: y1)
    (True,True,Just iGt) -> Just $ \(Envelope e0 e1) ->
      let x1 :+: y1 = fmap round (applyGeotransform iGt e0)
          x0 :+: y0 = fmap round (applyGeotransform iGt e1)
      in Envelope (x0 :+: y0) (x1 :+: y1)


writeBand
  :: forall s a. GDALType a
  => RWBand s a
  -> Envelope Int
  -> Size
  -> U.Vector (Value a)
  -> GDAL s ()
writeBand band win sz uvec =
  runConduit (yield (win, sz, uvec) =$= bandSink band)
{-# INLINEABLE writeBand #-}



bandSink
  :: forall s a. GDALType a
  => RWBand s a
  -> Sink (Envelope Int, Size, U.Vector (Value a)) (GDAL s) ()
bandSink band = lift (bandMaskType band) >>= \case
  MaskNoData -> do
    nd <- lift (noDataOrFail band)
    awaitForever $ \(win, sz, uvec) -> lift $
      write band win sz (toGVecWithNodata nd uvec)
  MaskAllValid ->
    awaitForever $ \(win, sz, uvec) -> lift $
      maybe (throwBindingException BandDoesNotAllowNoData)
            (write band win sz)
            (toGVec uvec)
  _ -> do
    mBand <- lift (bandMask band)
    awaitForever $ \(win, sz, uvec) -> lift $ do
      let (mask, vec) = toGVecWithMask uvec
      write band win sz vec
      write mBand win sz mask
  where

    write :: forall a'. GDALType a'
          => RWBand s a' -> Envelope Int -> Size -> St.Vector a' -> GDAL s ()
    write band' win sz@(bx :+: by) vec = do
      let sx :+: sy     = envelopeSize win
          xoff :+: yoff = envelopeMin win
      if sizeLen sz /= G.length vec
        then throwBindingException (InvalidRasterSize sz)
        else liftIO $
             St.unsafeWith vec $ \ptr ->
             checkCPLError "RasterIO" $
               {#call RasterIO as ^#}
                 (unBand band')
                 (fromEnumC GF_Write)
                 (fromIntegral xoff)
                 (fromIntegral yoff)
                 (fromIntegral sx)
                 (fromIntegral sy)
                 (castPtr ptr)
                 (fromIntegral bx)
                 (fromIntegral by)
                 (fromEnumC (hsDataType (Proxy :: Proxy a')))
                 0
                 0
{-# INLINEABLE bandSink #-}

unsafeBandDataset :: Band s a t -> Dataset s a t
unsafeBandDataset band = Dataset (Nothing, dsH) where
  dsH = {#call pure unsafe GDALGetBandDataset as ^#} (unBand band)

bandGeotransform :: MonadIO m => Band s a t -> m (Maybe Geotransform)
bandGeotransform = datasetGeotransform . unsafeBandDataset

bandProjection :: MonadIO m => Band s a t -> m (Maybe SpatialReference)
bandProjection = datasetProjection . unsafeBandDataset


bandSinkGeo
  :: forall s a. GDALType a
  => RWBand s a
  -> Sink (Envelope Double, Size, U.Vector (Value a)) (GDAL s) ()
bandSinkGeo band = do
  mGt <- bandGeotransform band
  case mGt >>= geoEnvelopeTransformer of
    Just trans -> CL.map (\(e,s,v) -> (trans e,s,v)) =$= bandSink band
    Nothing    -> lift (throwBindingException CannotInvertGeotransform)

noDataOrFail :: GDALType a => Band s a t -> GDAL s a
noDataOrFail = liftM (fromMaybe err) . bandNodataValue
  where
    err = error ("GDAL.readMasked: band has GMF_NODATA flag but did " ++
                 "not  return a nodata value")

bandMask :: Band s a t -> GDAL s (Band s Word8 t)
bandMask =
  liftIO . liftM (\p -> Band (p,GByte)) . {#call GetMaskBand as ^#}
         . unBand

data MaskType
  = MaskNoData
  | MaskAllValid
  | MaskPerBand
  | MaskPerDataset

bandMaskType :: Band s a t -> GDAL s MaskType
bandMaskType band = do
  flags <- liftIO ({#call unsafe GetMaskFlags as ^#} (unBand band))
  let testFlag f = (fromEnumC f .&. flags) /= 0
  return $ case () of
    () | testFlag GMF_NODATA      -> MaskNoData
    () | testFlag GMF_ALL_VALID   -> MaskAllValid
    () | testFlag GMF_PER_DATASET -> MaskPerDataset
    _                             -> MaskPerBand

maskFlagsForType :: MaskType -> CInt
maskFlagsForType MaskNoData      = fromEnumC GMF_NODATA
maskFlagsForType MaskAllValid    = fromEnumC GMF_ALL_VALID
maskFlagsForType MaskPerBand     = 0
maskFlagsForType MaskPerDataset  = fromEnumC GMF_PER_DATASET

{#enum define MaskFlag { GMF_ALL_VALID   as GMF_ALL_VALID
                       , GMF_PER_DATASET as GMF_PER_DATASET
                       , GMF_ALPHA       as GMF_ALPHA
                       , GMF_NODATA      as GMF_NODATA
                       } deriving (Eq,Bounded,Show) #}

copyBand
  :: Band s a t -> RWBand s a -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand src dst opts progress =
  liftIO $
  withProgressFun "copyBand" progress $ \pFunc ->
  withOptionList opts $ \o ->
  checkCPLError "copyBand" $
  {#call RasterBandCopyWholeRaster as ^#}
    (unBand src) (unBand dst) o pFunc nullPtr

fillBand :: GDALType a => Value a -> RWBand s a -> GDAL s ()
fillBand val b =
  runConduit (allBlocks b =$= CL.map (\i -> (i,v)) =$= blockSink b)
  where v = G.replicate (bandBlockLen b) val


foldl'
  :: forall s t a b. GDALType a
  => (b -> Value a -> b) -> b -> Band s a t -> GDAL s b
foldl' f = ifoldl' (\z _ v -> f z v)
{-# INLINEABLE foldl' #-}

ifoldl'
  :: forall s t a b. GDALType a
  => (b -> Pair Int -> Value a -> b) -> b -> Band s a t -> GDAL s b
ifoldl' f z band =
  runConduit (unsafeBlockSource band =$= CL.fold folder z)
  where
    !(mx :+: my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(nx :+: ny) = bandBlockCount band
    !(sx :+: sy) = bandBlockSize band
    folder !acc !(!(iB :+: jB), !vec) = go SPEC 0 0 acc
      where
        go !_ !i !j !acc'
          | i   < stopx = go SPEC (i+1) j
                          (f acc' ix (vec `G.unsafeIndex` (j*sx+i)))
          | j+1 < stopy = go SPEC 0 (j+1) acc'
          | otherwise   = acc'
          where ix = iB*sx+i :+: jB*sy+j
        !stopx
          | mx /= 0 && iB==nx-1 = mx
          | otherwise           = sx
        !stopy
          | my /= 0 && jB==ny-1 = my
          | otherwise           = sy
{-# INLINEABLE ifoldl' #-}

foldlWindow'
  :: forall s t a b. GDALType a
  => (b -> Value a -> b) -> b -> Band s a t -> Envelope Int -> GDAL s b
foldlWindow' f b = ifoldlWindow' (\z _ v -> f z v) b
{-# INLINEABLE foldlWindow' #-}

ifoldlWindow'
  :: forall s t a b. GDALType a
  => (b -> Pair Int -> Value a -> b)
  -> b -> Band s a t -> Envelope Int -> GDAL s b
ifoldlWindow' f z band (Envelope (x0 :+: y0) (x1 :+: y1)) = runConduit $
  allBlocks band =$= CL.filter inRange
                 =$= decorate (unsafeBlockConduit band)
                 =$= CL.fold folder z
  where
    inRange bIx = bx0 < x1 && x0 < bx1 && by0 < y1 &&  y0 < by1
      where
        !b0@(bx0 :+: by0) = bIx * bSize
        !(bx1 :+: by1)    = b0  + bSize
    !(mx :+: my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(nx :+: ny) = bandBlockCount band
    !bSize@(sx :+: sy) = bandBlockSize band
    folder !acc !(!(iB :+: jB), !vec) = go SPEC 0 0 acc
      where
        go !_ !i !j !acc'
          | i   < stopx = go SPEC (i+1) j acc''
          | j+1 < stopy = go SPEC 0 (j+1) acc'
          | otherwise   = acc'
          where
            !ix@(ixx :+: ixy) = iB*sx+i :+: jB*sy+j
            acc''
              | x0 <= ixx && ixx < x1 && y0 <= ixy && ixy < y1
              = f acc' ix (vec `G.unsafeIndex` (j*sx+i))
              | otherwise = acc'
        !stopx
          | mx /= 0 && iB==nx-1 = mx
          | otherwise           = sx
        !stopy
          | my /= 0 && jB==ny-1 = my
          | otherwise           = sy
{-# INLINEABLE ifoldlWindow' #-}

unsafeBlockSource
  :: GDALType a
  => Band s a t -> Source (GDAL s) (BlockIx, U.Vector (Value a))
unsafeBlockSource band = allBlocks band =$= decorate (unsafeBlockConduit band)
{-# INLINEABLE unsafeBlockSource #-}

blockSource
  :: GDALType a
  => Band s a t -> Source (GDAL s) (BlockIx, U.Vector (Value a))
blockSource band = allBlocks band =$= decorate (blockConduit band)
{-# INLINEABLE blockSource #-}

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> BlockIx  -> U.Vector (Value a) -> GDAL s ()
writeBandBlock band blockIx uvec =
  runConduit (yield (blockIx, uvec) =$= blockSink band)
{-# INLINEABLE writeBandBlock #-}

fmapBand
  :: forall s a b t. (GDALType a, GDALType b)
  => (Value a -> Value b)
  -> Band s a t
  -> RWBand s b 
  -> GDAL s ()
fmapBand f src dst
  | bandSize src /= bandSize dst
  = throwBindingException (InvalidRasterSize (bandSize src))
  | bandBlockSize src /= bandBlockSize dst
  = throwBindingException notImplementedErr
  | otherwise = runConduit
      (unsafeBlockSource src =$= CL.map (second (U.map f)) =$= blockSink dst)
  where
    notImplementedErr = NotImplemented
      "fmapBand: Not implemented for bands of different block size"
{-# INLINEABLE fmapBand #-}

foldBands
  :: forall s a b t. (GDALType a, GDALType b)
  => (Value b -> Value a -> Value b)
  -> RWBand s b
  -> [Band s a t]
  -> GDAL s ()
foldBands fun zb bs =
  runConduit (unsafeBlockSource zb =$= awaitForever foldThem =$= blockSink zb)
  where
    foldThem (bix, acc) = do
      r <- liftM (L.foldl' (G.zipWith fun) acc)
                 (lift (mapM (flip readBandBlock bix) bs))
      yield (bix, r)
{-# INLINEABLE foldBands #-}



blockSink
  :: forall s a. GDALType a
  => RWBand s a
  -> Sink (BlockIx, U.Vector (Value a)) (GDAL s) ()
blockSink band = do
  writeBlock <-
    if dtBand==dtBuf
      then return writeNative
      else do tempBuf <- liftIO (mallocForeignPtrBytes (len*szBand))
              return (writeTranslated tempBuf)
  lift (bandMaskType band) >>= \case
    MaskNoData   -> lift (noDataOrFail band) >>= sinkNodata   writeBlock
    MaskAllValid ->                              sinkAllValid writeBlock
    _            -> lift (bandMask band)     >>= sinkMask     writeBlock

  where
    len     = bandBlockLen band
    dtBand  = bandDataType (band)
    szBand  = sizeOfDataType dtBand
    dtBuf   = hsDataType (Proxy :: Proxy a)
    szBuf   = sizeOfDataType dtBuf

    sinkNodata writeBlock nd = awaitForever $ \(ix, vec) -> liftIO $ do
      checkLen vec
      St.unsafeWith (toGVecWithNodata nd vec) (writeBlock ix)


    sinkAllValid writeBlock = awaitForever $ \(ix, vec) -> liftIO $ do
      checkLen vec
      case toGVec vec of
        Just buf -> St.unsafeWith buf (writeBlock ix)
        Nothing  -> throwBindingException BandDoesNotAllowNoData

    sinkMask writeBlock bMask = awaitForever $ \(ix, vec) -> liftIO $ do
      checkLen vec
      let (mask, vec') = toGVecWithMask vec
          off          = bi * bs
          win          = liftA2 min bs (rs - off)
          bi           = fmap fromIntegral ix
          bs           = fmap fromIntegral (bandBlockSize band)
          rs           = fmap fromIntegral (bandSize band)
      St.unsafeWith vec' (writeBlock ix)
      St.unsafeWith mask $ \pBuf ->
        checkCPLError "WriteBlock" $
        {#call RasterIO as ^#}
          (unBand bMask)
          (fromEnumC GF_Write)
          (pFst off)
          (pSnd off)
          (pFst win)
          (pSnd win)
          (castPtr pBuf)
          (pFst win)
          (pSnd win)
          (fromEnumC GByte)
          0
          (pFst bs * fromIntegral (sizeOfDataType GByte))

    checkLen v
      | len /= G.length v
      = throwBindingException (InvalidBlockSize (G.length v))
      | otherwise = return ()

    writeTranslated temp blockIx pBuf =
      withForeignPtr temp $ \pTemp -> do
        {#call unsafe CopyWords as ^#}
          (castPtr pTemp)
          (fromEnumC dtBand)
          (fromIntegral szBand)
          (castPtr pBuf)
          (fromEnumC dtBuf)
          (fromIntegral szBuf)
          (fromIntegral len)
        writeNative blockIx pTemp

    writeNative blockIx pBuf =
      checkCPLError "WriteBlock" $
      {#call GDALWriteBlock as ^#}
        (unBand band)
        (pFst bi)
        (pSnd bi)
        (castPtr pBuf)
      where bi = fmap fromIntegral blockIx
{-# INLINEABLE blockSink #-}


readBandBlock
  :: forall s t a. GDALType a
  => Band s a t -> BlockIx -> GDAL s (U.Vector (Value a))
readBandBlock band blockIx = liftM fromJust $ runConduit $
  yield blockIx =$= unsafeBlockConduit band =$= CL.head
{-# INLINEABLE readBandBlock #-}


allBlocks :: Monad m => Band s a t -> Producer m BlockIx
allBlocks band = CL.sourceList [x :+: y | y <- [0..ny-1], x <- [0..nx-1]]
  where
    !(nx :+: ny) = bandBlockCount band
{-# INLINEABLE allBlocks #-}

blockConduit
  :: forall s a t. GDALType a
  => Band s a t
  -> Conduit BlockIx (GDAL s) (U.Vector (Value a))
blockConduit band =
  unsafeBlockConduitM band =$= CL.mapM (liftIO . U.freeze)
{-# INLINEABLE blockConduit #-}

unsafeBlockConduit
  :: forall s a t. GDALType a
  => Band s a t
  -> Conduit BlockIx (GDAL s) (U.Vector (Value a))
unsafeBlockConduit band =
  unsafeBlockConduitM band =$= CL.mapM (liftIO . U.unsafeFreeze)
{-# INLINEABLE unsafeBlockConduit #-}


unsafeBlockConduitM
  :: forall s a t. GDALType a
  => Band s a t
  -> Conduit BlockIx (GDAL s) (UM.IOVector (Value a))
unsafeBlockConduitM band = do
  buf <- liftIO (M.new len)
  maskType <- lift $ bandMaskType band
  case maskType of
    MaskNoData -> do
      noData <- lift $ noDataOrFail band
      blockReader (mkValueUMVector noData buf) buf (const (return ()))
    MaskAllValid ->
      blockReader (mkAllValidValueUMVector buf) buf (const (return ()))
    _ -> do
      mBuf <- liftIO $ M.replicate len 0
      mask <- lift $ bandMask band
      blockReader (mkMaskedValueUMVector mBuf buf) buf (readMask mask mBuf)
  where
    isNative = dtBand == dtBuf
    len      = bandBlockLen band
    dtBand   = bandDataType band
    dtBuf    = hsDataType (Proxy :: Proxy a)

    blockReader vec buf extra
      | isNative = awaitForever $ \ix -> do
          liftIO (Stm.unsafeWith buf (loadBlock ix))
          extra ix
          yield vec
      | otherwise = do
          temp <- liftIO $ mallocForeignPtrBytes (len*szBand)
          awaitForever $ \ix -> do
            liftIO $ withForeignPtr temp $ \pTemp -> do
              loadBlock ix pTemp
              Stm.unsafeWith buf (translateBlock pTemp)
            extra ix
            yield vec

    loadBlock ix pBuf = 
      checkCPLError "ReadBlock" $
      {#call ReadBlock as ^#}
        (unBand band)
        (fromIntegral (pFst ix))
        (fromIntegral (pSnd ix))
        (castPtr pBuf)

    translateBlock pTemp pBuf  = 
      {#call unsafe CopyWords as ^#}
        (castPtr pTemp)
        (fromEnumC dtBand)
        (fromIntegral szBand)
        (castPtr pBuf)
        (fromEnumC dtBuf)
        (fromIntegral szBuf)
        (fromIntegral len)
    szBand = sizeOfDataType dtBand
    szBuf = sizeOfDataType dtBuf

    readMask mask vMask ix =
      liftIO $
      Stm.unsafeWith vMask $ \pMask ->
      checkCPLError "RasterIO" $
      {#call RasterIO as ^#}
        (unBand mask)
        (fromEnumC GF_Read)
        (pFst off)
        (pSnd off)
        (pFst win)
        (pSnd win)
        (castPtr pMask)
        (pFst win)
        (pSnd win)
        (fromEnumC GByte)
        0
        (pFst bs)
      where
        bi  = fmap fromIntegral ix
        off = bi * bs
        win = liftA2 min bs (rs  - off)
    bs  = fmap fromIntegral (bandBlockSize band)
    rs  = fmap fromIntegral (bandSize band)
{-# INLINEABLE unsafeBlockConduitM #-}

openDatasetCount :: IO Int
openDatasetCount =
  alloca $ \ppDs ->
  alloca $ \pCount -> do
    {#call unsafe GetOpenDatasets as ^#} ppDs pCount
    liftM fromIntegral (peek pCount)

-----------------------------------------------------------------------------
-- Metadata
-----------------------------------------------------------------------------

metadataDomains
  :: (MonadIO m, MajorObject o t)
  => o t -> m [Text]
#if SUPPORTS_METADATA_DOMAINS
metadataDomains o =
  liftIO $
  fromCPLStringList $
  {#call unsafe GetMetadataDomainList as ^#} (majorObject o)
#else
metadataDomains = const (return [])
#endif

metadata
  :: (MonadIO m, MajorObject o t)
  => Maybe ByteString -> o t -> m [(Text,Text)]
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
  :: (MonadIO m, MajorObject o t)
  => Maybe ByteString -> ByteString -> o t -> m (Maybe ByteString)
metadataItem domain key o =
  liftIO $
  useAsCString key $ \pKey ->
  withMaybeByteString domain $
    {#call unsafe GetMetadataItem as ^#} (majorObject o) pKey >=>
      maybePackCString

setMetadataItem
  :: (MonadIO m, MajorObject o t, t ~ ReadWrite)
  => Maybe ByteString -> ByteString -> ByteString -> o t -> m ()
setMetadataItem domain key val o =
  liftIO $
  checkCPLError "setMetadataItem" $
  useAsCString key $ \pKey ->
  useAsCString val $ \pVal ->
  withMaybeByteString domain $
    {#call unsafe GDALSetMetadataItem as ^#} (majorObject o) pKey pVal

description :: (MonadIO m, MajorObject o t) => o t -> m ByteString
description =
  liftIO . ({#call unsafe GetDescription as ^#} . majorObject >=> packCString)

setDescription
  :: (MonadIO m, MajorObject o t, t ~ ReadWrite)
  => ByteString -> o t -> m ()
setDescription val =
  liftIO . checkGDALCall_ const .
  useAsCString val . {#call unsafe GDALSetDescription as ^#} . majorObject



-----------------------------------------------------------------------------
-- Conduit utils
-----------------------------------------------------------------------------

-- | Parallel zip of two conduits
pZipConduitApp
  :: Monad m
  => Conduit a m (b -> c)
  -> Conduit a m b
  -> Conduit a m c
pZipConduitApp (ConduitM l0) (ConduitM r0) = ConduitM $ \rest -> let
  go (HaveOutput p c o) (HaveOutput p' c' o') =
    HaveOutput (go p p') (c>>c') (o o')
  go (NeedInput p c) (NeedInput p' c') =
    NeedInput (\i -> go (p i) (p' i)) (\u -> go (c u) (c' u))
  go (Done ()) (Done ())  = rest ()
  go (PipeM m) (PipeM m') = PipeM (liftM2 go m m')
  go (Leftover p i) (Leftover p' _) = Leftover (go p p') i
  go _              _               = error "pZipConduitApp: not parallel"
  in go (injectLeftovers $ l0 Done) (injectLeftovers $ r0 Done)

newtype PZipConduit i m o =
  PZipConduit { getPZipConduit :: Conduit i m o}

instance Monad m => Functor (PZipConduit i m) where
    fmap f = PZipConduit . mapOutput f . getPZipConduit

instance Monad m => Applicative (PZipConduit i m) where
    pure = PZipConduit . forever . yield
    PZipConduit l <*> PZipConduit r = PZipConduit (pZipConduitApp l r)

zipBlocks
  :: GDALType a
  => Band s a t -> PZipConduit BlockIx (GDAL s) (U.Vector (Value a))
zipBlocks = PZipConduit . unsafeBlockConduit

getZipBlocks
  :: PZipConduit BlockIx (GDAL s) a
  -> Conduit BlockIx (GDAL s) (BlockIx, a)
getZipBlocks = decorate . getPZipConduit

decorate :: Monad m => Conduit a m b -> Conduit a m (a, b)
decorate (ConduitM c0) = ConduitM $ \rest -> let
  go1 HaveOutput{} = error "unexpected NeedOutput"
  go1 (NeedInput p c) = NeedInput (\i -> go2 i (p i)) (go1 . c)
  go1 (Done r) = rest r
  go1 (PipeM mp) = PipeM (liftM (go1) mp)
  go1 (Leftover p i) = Leftover (go1 p) i

  go2 i (HaveOutput p c o) = HaveOutput (go2 i p) c (i, o)
  go2 _ (NeedInput p c)    = NeedInput (\i -> go2 i (p i)) (go1 . c)
  go2 _ (Done r)           = rest r
  go2 i (PipeM mp)         = PipeM (liftM (go2 i) mp)
  go2 i (Leftover p i')    = Leftover (go2 i p) i'
  in go1 (c0 Done)
{-# INLINEABLE decorate #-}
