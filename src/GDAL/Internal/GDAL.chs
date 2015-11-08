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
  , Driver (..)
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

  , bandTypedAs
  , bandCoercedTo

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
  , fillBand
  , readBand
  , createBandMask
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

  , module GDAL.Internal.DataType
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.Exception (Exception(..))
import Control.Monad (liftM, liftM2, when, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Bits ((.&.))
import Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)

import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (toBool, fromBool, with)

import GHC.Types (SPEC(..))

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Types
import GDAL.Internal.Types.Value
import qualified GDAL.Internal.Types.Vector as GV
import qualified GDAL.Internal.Types.Vector.Mutable as GVM
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

newtype Driver = Driver ByteString
  deriving (Eq, IsString)

instance Show Driver where
  show (Driver s) = show s

version :: (Int, Int)
version = ({#const GDAL_VERSION_MAJOR#} , {#const GDAL_VERSION_MINOR#})

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDriverOptions
  | UnknownRasterDataType
  | UnsupportedRasterDataType !DataType
  | NullDataset
  | NullBand
  | UnknownDriver !ByteString
  | BandDoesNotAllowNoData
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
create drv path (XY nx ny) bands dtype  options
  | dtype == gdtUnknown = throwBindingException UnknownRasterDataType
  | otherwise
  = newDatasetHandle $ withCString path $ \path' -> do
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

bandNodataValue :: GDALType a => Band s a t -> GDAL s (Maybe a)
bandNodataValue b =
  liftIO $
  alloca $ \p -> do
    value <- {#call unsafe GetRasterNoDataValue as ^#} (unBand b) p
    hasNodata <- liftM toBool $ peek p
    return (if hasNodata then Just (convertGType value) else Nothing)
{-# INLINE bandNodataValue #-}


setBandNodataValue :: GDALType a => RWBand s a -> a -> GDAL s ()
setBandNodataValue b v =
  liftIO $
  checkCPLError "SetRasterNoDataValue" $
  {#call unsafe SetRasterNoDataValue as ^#} (unBand b) (convertGType v)

createBandMask :: RWBand s a -> MaskType -> GDAL s ()
createBandMask band maskType = liftIO $
  checkCPLError "createBandMask" $
  {#call CreateMaskBand as ^#} (unBand band) cflags
  where cflags = maskFlagsForType maskType

readBand :: forall s t a. GDALType a
  => (Band s a t)
  -> Envelope Int
  -> Size
  -> GDAL s (Vector (Value a))
readBand band win (XY bx by) =
  bandMaskType band >>= \case
    MaskNoData ->
      liftM2 mkValueUVector (noDataOrFail band) (read_ band)
    MaskAllValid ->
      liftM mkAllValidValueUVector (read_ band)
    _ ->
      liftM2 mkMaskedValueUVector (read_ =<< bandMask band) (read_ band)
  where
    XY sx sy     = envelopeSize win
    XY xoff yoff = envelopeMin win
    read_ :: forall a'. GDALType a' => Band s a' t -> GDAL s (GV.Vector a')
    read_ b = liftIO $ do
      vec <- GM.new (bx*by)
      GVM.unsafeWithDataType vec $ \dtype ptr -> do
        checkCPLError "RasterAdviseRead" $
          {#call unsafe RasterAdviseRead as ^#}
            (unBand b)
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            (fromIntegral bx)
            (fromIntegral by)
            (fromEnumC dtype)
            nullPtr
        checkCPLError "RasterIO" $
          {#call RasterIO as ^#}
            (unBand b)
            (fromEnumC GF_Read)
            (fromIntegral xoff)
            (fromIntegral yoff)
            (fromIntegral sx)
            (fromIntegral sy)
            ptr
            (fromIntegral bx)
            (fromIntegral by)
            (fromEnumC dtype)
            0
            0
      G.unsafeFreeze vec
{-# INLINE readBand #-}


writeMasked
  :: GDALType a
  => RWBand s a
  -> (RWBand s a -> GV.Vector a -> GDAL s ())
  -> (RWBand s Word8 -> GV.Vector Word8 -> GDAL s ())
  -> Vector (Value a)
  -> GDAL s ()
writeMasked band writer maskWriter uvec =
  bandMaskType band >>= \case
    MaskNoData ->
      noDataOrFail band >>= writer band . flip toGVecWithNodata uvec
    MaskAllValid ->
      maybe (throwBindingException BandDoesNotAllowNoData)
            (writer band)
            (toGVec uvec)
    _ ->
      let (mask, vec) = toGVecWithMask uvec
      in writer band vec >> bandMask band >>= flip maskWriter mask

noDataOrFail :: GDALType a => Band s a t -> GDAL s a
noDataOrFail = liftM (fromMaybe err) . bandNodataValue
  where
    err = error ("GDAL.readMasked: band has GMF_NODATA flag but did " ++
                 "not  return a nodata value")

bandMask :: Band s a t -> GDAL s (Band s Word8 t)
bandMask = liftIO . liftM Band . {#call GetMaskBand as ^#} . unBand

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


writeBand
  :: forall s a. GDALType a
  => RWBand s a
  -> Envelope Int
  -> Size
  -> Vector (Value a)
  -> GDAL s ()
writeBand band win sz@(XY bx by) = writeMasked band write write
  where
    write :: forall a'. GDALType a'
         => RWBand s a' -> GV.Vector a' -> GDAL s ()
    write band' vec = do
      let XY sx sy     = envelopeSize win
          XY xoff yoff = envelopeMin win
      if sizeLen sz /= G.length vec
        then throwBindingException (InvalidRasterSize sz)
        else liftIO $
             GV.unsafeWithDataType vec $ \dt ptr ->
             checkCPLError "RasterIO" $
               {#call RasterIO as ^#}
                 (unBand band')
                 (fromEnumC GF_Write)
                 (fromIntegral xoff)
                 (fromIntegral yoff)
                 (fromIntegral sx)
                 (fromIntegral sy)
                 ptr
                 (fromIntegral bx)
                 (fromIntegral by)
                 (fromEnumC dt)
                 0
                 0
{-# INLINE writeBand #-}

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
  => (b -> Value a -> GDAL s b) -> b -> Band s a t -> GDAL s b
foldlM' f = ifoldlM' (\acc _ -> f acc)
{-# INLINE foldlM' #-}

ifoldlM'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> GDAL s b) -> b -> Band s a t -> GDAL s b
ifoldlM' f initialAcc band = do
  (load, vec) <- mkBlockLoader band
  ifoldlM_loop load vec
  where
    !(XY mx my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(XY nx ny) = bandBlockCount band
    !(XY sx sy) = bandBlockSize band
    {-# INLINE ifoldlM_loop #-}
    ifoldlM_loop loadBlock vec = goB SPEC 0 0 initialAcc
      where
        goB !sPEC !iB !jB !acc
          | iB < nx = do loadBlock (XY iB jB)
                         go SPEC 0 0 acc >>= goB sPEC (iB+1) jB
          | jB+1 < ny = goB sPEC 0 (jB+1) acc
          | otherwise = return acc
          where
            go !sPEC2 !i !j !acc'
              | i   < stopx = do
                  !v <- liftIO (UM.unsafeRead vec (j*sx+i))
                  f acc' ix v >>= go sPEC2 (i+1) j
              | j+1 < stopy = go sPEC2 0 (j+1) acc'
              | otherwise   = return acc'
              where ix = XY (iB*sx+i) (jB*sy+j)
            !stopx
              | mx /= 0 && iB==nx-1 = mx
              | otherwise           = sx
            !stopy
              | my /= 0 && jB==ny-1 = my
              | otherwise           = sy
{-# INLINE ifoldlM' #-}

writeBandBlock
  :: forall s a. GDALType a
  => RWBand s a -> BlockIx  -> Vector (Value a) -> GDAL s ()
writeBandBlock band blockIx uvec = do
  when (bandBlockLen band /= len) $
    throwBindingException (InvalidBlockSize len)
  writeMasked band write writeMask uvec
  where
    len = G.length uvec
    bi  = fmap fromIntegral blockIx

    write band' vec =
      liftIO $
      GV.unsafeAsDataType (bandDataType band') vec $ \pVec ->
      checkCPLError "WriteBlock" $
      {#call WriteBlock as ^#}
        (unBand band')
        (px bi)
        (py bi)
        pVec

    bs  = fmap fromIntegral (bandBlockSize band)
    rs  = fmap fromIntegral (bandSize band)
    off = bi * bs
    win = liftA2 min bs (rs - off)

    writeMask band' vec =
      liftIO $
      checkCPLError "WriteBlock" $
      GV.unsafeWithDataType vec $ \dt pVec ->
        {#call RasterIO as ^#}
          (unBand band')
          (fromEnumC GF_Write)
          (px off)
          (py off)
          (px win)
          (py win)
          pVec
          (px win)
          (py win)
          (fromEnumC dt)
          0
          (px bs * fromIntegral (sizeOfDataType dt))
{-# INLINE writeBandBlock #-}

readBandBlock
  :: forall s t a. GDALType a
  => Band s a t -> BlockIx -> GDAL s (Vector (Value a))
readBandBlock band blockIx = do
  (load, vec) <- mkBlockLoader band
  load blockIx
  liftIO $ G.unsafeFreeze vec
{-# INLINE readBandBlock #-}


mkBlockLoader
  :: forall s t a. GDALType a
  => Band s a t
  -> GDAL s (BlockIx -> GDAL s (), UM.IOVector (Value a))
mkBlockLoader band = do
  buf <- liftIO $ GVM.newAs (bandDataType band) len
  bandMaskType band >>= \case
    MaskNoData -> do
      noData <- noDataOrFail band
      return (blockLoader buf, mkValueUMVector noData buf)
    MaskAllValid ->
      return (blockLoader buf, mkAllValidValueUMVector buf)
    _ -> do
      maskBuf <- liftIO $ GM.replicate len 0
      mask <- bandMask band
      return ( maskedBlockLoader mask maskBuf (blockLoader buf)
             , mkMaskedValueUMVector maskBuf buf)
  where
    len = bandBlockLen band

    maskedBlockLoader mask maskBuf loadBlock blockIx = do
      loadBlock blockIx
      liftIO $ do
        GVM.unsafeWithDataType maskBuf $ \dt pVec ->
          checkCPLError "RasterIO" $
          {#call RasterIO as ^#}
            (unBand mask)
            (fromEnumC GF_Read)
            (px off)
            (py off)
            (px win)
            (py win)
            pVec
            (px win)
            (py win)
            (fromEnumC dt)
            0
            (px bs * fromIntegral (sizeOfDataType dt))
      where
        bi  = fmap fromIntegral blockIx
        bs  = fmap fromIntegral (bandBlockSize band)
        rs  = fmap fromIntegral (bandSize band)
        off = bi * bs
        win = liftA2 min bs (rs  - off)

    blockLoader buf blockIx = liftIO $ do
      GVM.unsafeWithDataType buf $ \_ pVec ->
        checkCPLError "ReadBlock" $
        {#call ReadBlock as ^#}
          (unBand band)
          (px bi)
          (py bi)
          pVec
      where
        bi  = fmap fromIntegral blockIx
{-# INLINE mkBlockLoader #-}

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


fillBand :: GDALType a => Value a -> RWBand s a -> GDAL s ()
fillBand v band =
  mapM_ (flip (writeBandBlock band) vec) [XY i j | j<-[0..ny-1], i<-[0..nx-1]]
  where
    XY nx ny = bandBlockCount band
    vec      = G.replicate (bandBlockLen band) v
