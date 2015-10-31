{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE ExistentialQuantification #-}

module GDAL.Internal.GDAL (
    GDALType (..)
  , GDALRasterException (..)
  , DataType (..)
  , Geotransform (..)
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
  , nullDatasetH
  , allRegister
  , destroyDriverManager
  , create
  , createMem
  , flushCache
  , closeDataset
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy
  , driverCreationOptionList

  , dataTypeSize
  , dataTypeByName
  , dataTypeUnion
  , dataTypeIsComplex

  , datasetSize
  , reifyDataType
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetBandCount

  , bandDataType
  , reifyBandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandSize
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , readBand
  , readBandBlock
  , writeBand
  , writeBandBlock

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'

  , unDataset
  , unBand
  , version
  , newDatasetHandle
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), throw)
import Control.Monad (liftM, liftM2, when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Int (Int16, Int32)
import Data.Bits ((.&.))
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Complex (Complex(..), realPart)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (toBool, fromBool, with)

import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Data.Char (toUpper)

import GDAL.Internal.Types
import GDAL.Internal.Util
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLProgress#}
import GDAL.Internal.OGRError
import GDAL.Internal.OSR
import GDAL.Internal.OGRGeometry (Envelope(..), envelopeSize)


#include "gdal.h"

$(let names = fmap (words . map toUpper) $
                readProcess "gdal-config" ["--formats"] ""
  in createEnum "Driver" names)

version :: (Int, Int)
version =
  ( {#const GDAL_VERSION_MAJOR#}
  , {#const GDAL_VERSION_MINOR#})

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDataType   !DataType
  | InvalidProjection !OGRException
  | InvalidDriverOptions
  | CopyStopped
  | UnknownRasterDataType
  | UnsupportedRasterDataType !DataType
  | NullDataset
  | NullBand
  deriving (Typeable, Show, Eq)

instance NFData GDALRasterException where
  rnf a = a `seq` () -- All fields are already strict so no need to rnf them

instance Exception GDALRasterException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException


class (Storable a, Show a, Typeable a) => GDALType a where
  dataType :: Proxy a -> DataType
  -- | default nodata value when writing to bands with no datavalue set
  nodata   :: a
  -- | how to convert to double for use in setBandNodataValue
  toCDouble :: a -> CDouble
  -- | how to convert from double for use with bandNodataValue
  fromCDouble :: CDouble -> a

  fillBand :: a -> RWBand s -> GDAL s ()
  fillBand v = fillRaster (toCDouble v) (toCDouble v)
  {-# INLINE fillBand #-}

{#enum DataType {} omit (GDT_TypeCount) deriving (Eq, Show, Bounded) #}

instance NFData DataType where
  rnf a = a `seq`()

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

{#pointer DatasetH newtype #}

nullDatasetH :: DatasetH
nullDatasetH = DatasetH nullPtr

deriving instance Eq DatasetH

newtype Dataset s (t::AccessMode) =
  Dataset (ReleaseKey, DatasetH)

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

newtype Band s (t::AccessMode) =
  Band { unBand :: RasterBandH }

reifyBandDataType
  :: Band s t -> (forall a. GDALType a => Proxy a -> b) -> b
reifyBandDataType b = reifyDataType (bandDataType b)

type ROBand s = Band s ReadOnly
type RWBand s = Band s ReadWrite


{#pointer DriverH newtype#}

{#fun AllRegister as ^ {} -> `()'  #}

{#fun DestroyDriverManager as ^ {} -> `()'#}

driverByName :: Driver -> IO DriverH
driverByName s = withCString (show s) {#call unsafe GetDriverByName as ^#}

driverCreationOptionList :: Driver -> String
driverCreationOptionList driver = unsafePerformIO $ do
  d <- driverByName driver
  {#call GetDriverCreationOptionList	as ^#} d >>= peekCString

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
  withProgressFun CopyStopped progressFun $ \pFunc -> do
    d <- driverByName driver
    withOptionList options $ \o -> do
      validateCreationOptions d o
      withCString path $ \p ->
        {#call GDALCreateCopy as ^#}
          d p (unDataset ds) (fromBool strict) o pFunc (castPtr nullPtr)


newDatasetHandle :: IO DatasetH -> GDAL s (Dataset s t)
newDatasetHandle act =
  liftM Dataset $ allocate (checkGDALCall checkit act) {#call GDALClose as ^#}
  where
    checkit exc p
      | p==nullDatasetH = Just (fromMaybe
                                (GDALBindingException NullDataset) exc)
      | otherwise       = Nothing


createMem
  :: Size -> Int -> DataType -> OptionList -> GDAL s (Dataset s ReadWrite)
createMem = create MEM ""

flushCache :: forall s. RWDataset s -> GDAL s ()
flushCache = liftIO . {#call GDALFlushCache as ^#} . unDataset

datasetSize :: Dataset s t -> Size
datasetSize ds =
  fmap fromIntegral $
     XY ({#call pure unsafe GetRasterXSize as ^#} (unDataset ds))
        ({#call pure unsafe GetRasterYSize as ^#} (unDataset ds))

datasetProjection :: Dataset s t -> GDAL s (Maybe SpatialReference)
datasetProjection ds = liftIO $ do
  srs <- {#call unsafe GetProjectionRef as ^#} (unDataset ds)
  c <- peek srs
  if c == 0
    then return Nothing
    else srsFromWktIO srs >>=
          either (throwBindingException . InvalidProjection)
                 (return . Just)


setDatasetProjection :: RWDataset s -> SpatialReference -> GDAL s ()
setDatasetProjection ds srs =
  liftIO $ checkCPLError "SetProjection" $
    unsafeUseAsCString (srsToWkt srs)
      ({#call unsafe SetProjection as ^#} (unDataset ds))

data Geotransform
  = Geotransform {
      gtXOff   :: !Double
    , gtXDelta :: !Double
    , gtXRot   :: !Double
    , gtYOff   :: !Double
    , gtYRot   :: !Double
    , gtYDelta :: !Double
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

getBand :: Int -> Dataset s t -> GDAL s (Band s t)
getBand b ds =
  liftIO $
  liftM Band $
  checkGDALCall checkit $
  {#call GetRasterBand as ^#} (unDataset ds) (fromIntegral b)
  where
    checkit exc p
      | p == nullBandH = Just (fromMaybe (GDALBindingException NullBand) exc)
      | otherwise      = Nothing


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




bandDataType :: Band s t -> DataType
bandDataType = toEnumC . {#call pure unsafe GetRasterDataType as ^#} . unBand

bandBlockSize :: (Band s t) -> Size
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   {#call unsafe GetBlockSize as ^#} (unBand band) xPtr yPtr
   liftM (fmap fromIntegral) (liftM2 XY (peek xPtr) (peek yPtr))

bandBlockLen :: Band s t -> Int
bandBlockLen = (\(XY x y) -> x*y) . bandBlockSize

bandSize :: Band s a -> Size
bandSize band =
  fmap fromIntegral $
    XY ({#call pure unsafe GetRasterBandXSize as ^#} (unBand band))
       ({#call pure unsafe GetRasterBandYSize as ^#} (unBand band))

allBand :: Band s a -> Envelope Int
allBand = Envelope (pure 0) . bandSize

bandBlockCount :: Band s t -> XY Int
bandBlockCount b = fmap ceiling $ liftA2 ((/) :: Double -> Double -> Double)
                     (fmap fromIntegral (bandSize b))
                     (fmap fromIntegral (bandBlockSize b))

checkType
  :: GDALType a => Band s t -> Proxy a -> GDAL s ()
checkType b p
  | rt == bt  = return ()
  | otherwise = throwBindingException (InvalidDataType bt)
  where rt = dataType p
        bt = bandDataType b

bandNodataValue :: GDALType a => Band s t -> GDAL s (Maybe a)
bandNodataValue b =
  liftM (fmap fromCDouble) (liftIO (bandNodataValueIO (unBand b)))

bandNodataValueIO :: RasterBandH -> IO (Maybe CDouble)
bandNodataValueIO b = alloca $ \p -> do
   value <- {#call unsafe GetRasterNoDataValue as ^#} b p
   hasNodata <- liftM toBool $ peek p
   return (if hasNodata then Just value else Nothing)
{-# INLINE bandNodataValueIO #-}


setBandNodataValue :: GDALType a => RWBand s -> a -> GDAL s ()
setBandNodataValue b v =
  liftIO $
    checkCPLError "SetRasterNoDataValue" $
      {#call unsafe SetRasterNoDataValue as ^#} (unBand b) (toCDouble v)

fillRaster :: CDouble -> CDouble  -> RWBand s -> GDAL s ()
fillRaster r i b =
  liftIO $
    checkCPLError "FillRaster" $
      {#call GDALFillRaster as ^#} (unBand b) r i


readBand :: forall s t a. GDALType a
  => (Band s t)
  -> Envelope Int
  -> Size
  -> GDAL s (Vector (Value a))
readBand band win size = liftIO $ readBandIO band win size
{-# INLINE readBand #-}

readBandIO :: forall s t a. GDALType a
  => (Band s t)
  -> Envelope Int
  -> Size
  -> IO (Vector (Value a))
readBandIO band win (XY bx by) = readMasked band read_
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
{-# INLINE readBandIO #-}

readMasked
  :: GDALType a
  => Band s t
  -> (forall a'. GDALType a' => RasterBandH -> IO (St.Vector a'))
  -> IO (Vector (Value a))
readMasked band reader = do
  flags <- {#call unsafe GetMaskFlags as ^#} (unBand band)
  reader (unBand band) >>= fmap stToUValue . mask flags (unBand band)
  where
    mask fs
      | hasFlag fs MaskPerDataset = useMaskBand
      | hasFlag fs MaskNoData     = useNoData
      | hasFlag fs MaskAllValid   = useAsIs
      | otherwise                 = useMaskBand
    hasFlag fs f = fromEnumC f .&. fs == fromEnumC f
    useAsIs _ = return . St.map Value
    useNoData bPtr vs = do
      mNodata <- bandNodataValueIO bPtr
      let toValue = case mNodata of
                      Nothing -> Value
                      Just nd ->
                        \v -> if toCDouble v == nd then NoData else Value v
      return (St.map toValue vs)
    useMaskBand bPtr vs = do
      ms <- {#call GetMaskBand as ^#} bPtr >>= reader :: IO (St.Vector Word8)
      return $ St.zipWith (\v m -> if m/=0 then Value v else NoData) vs ms
{-# INLINE readMasked #-}

{#enum define MaskFlag { GMF_ALL_VALID   as MaskAllValid
                       , GMF_PER_DATASET as MaskPerDataset
                       , GMF_ALPHA       as MaskAlpha
                       , GMF_NODATA      as MaskNoData
                       } deriving (Eq,Bounded,Show) #}


writeBand :: forall s a. GDALType a
  => RWBand s
  -> Envelope Int
  -> Size
  -> Vector (Value a)
  -> GDAL s ()
writeBand band win sz@(XY bx by) uvec = liftIO $ do
  bNodata <- fmap (maybe nodata fromCDouble) (bandNodataValueIO (unBand band))
  let nElems    = bx * by
      (fp, len) = St.unsafeToForeignPtr0 vec
      vec       = St.map (fromValue bNodata) (uToStValue uvec)
      XY sx sy     = envelopeSize win
      XY xoff yoff = envelopeMin win
  if nElems /= len
    then throwBindingException (InvalidRasterSize sz)
    else withForeignPtr fp $ \ptr ->
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
  => Band s t -> BlockIx -> GDAL s (Vector (Value a))
readBandBlock band blockIx = do
  checkType band (Proxy :: Proxy a)
  liftIO $ readMasked band $ \b -> do
    vec <- Stm.new (bandBlockLen band)
    Stm.unsafeWith vec $
      checkCPLError "ReadBlock" . {#call ReadBlock as ^#} b x y . castPtr
    St.unsafeFreeze vec
  where XY x y = fmap fromIntegral blockIx
{-# INLINE readBandBlock #-}

foldl'
  :: forall s t a b. GDALType a
  => (b -> Value a -> b) -> b -> Band s t -> GDAL s b
foldl' f = foldlM' (\acc -> return . f acc)
{-# INLINE foldl' #-}

ifoldl'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> b) -> b -> Band s t -> GDAL s b
ifoldl' f = ifoldlM' (\acc ix -> return . f acc ix)
{-# INLINE ifoldl' #-}

foldlM'
  :: forall s t a b. GDALType a
  => (b -> Value a -> IO b) -> b -> Band s t -> GDAL s b
foldlM' f = ifoldlM' (\acc _ -> f acc)
{-# INLINE foldlM' #-}

ifoldlM'
  :: forall s t a b. GDALType a
  => (b -> BlockIx -> Value a -> IO b) -> b -> Band s t -> GDAL s b
ifoldlM' f initialAcc band = do
  checkType band (Proxy :: Proxy a)
  liftIO $ do
    mNodata <- bandNodataValueIO (unBand band)
    allocaArray (sx*sy) $ \ptr -> do
      let toValue = case mNodata of
                      Nothing -> Value
                      Just nd ->
                        \v -> if toCDouble v == nd then NoData else Value v
          goB !iB !jB !acc
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
  => RWBand s -> BlockIx  -> Vector (Value a) -> GDAL s ()
writeBandBlock band blockIx uvec = do
  checkType band (Proxy :: Proxy a)
  liftIO $ do
    bNodata <- fmap (maybe nodata fromCDouble) (bandNodataValueIO pBand)
    let (fp, len) = St.unsafeToForeignPtr0 vec
        vec       = St.map (fromValue bNodata) (uToStValue uvec)
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



instance GDALType Word8 where
  dataType _ = GDT_Byte
  nodata = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  dataType _ = GDT_UInt16
  nodata = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  dataType _ = GDT_UInt32
  nodata = maxBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  dataType _ = GDT_Int16
  nodata = minBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  dataType _ = GDT_Int32
  nodata = minBound
  toCDouble = fromIntegral
  fromCDouble = truncate
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  dataType _ = GDT_Float32
  nodata = 0/0
  toCDouble = realToFrac
  fromCDouble = realToFrac
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  dataType _ = GDT_Float64
  nodata = 0/0
  toCDouble = realToFrac
  fromCDouble = realToFrac
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

#ifdef STORABLE_COMPLEX
instance GDALType (Complex Int16) where
  dataType _ = GDT_CInt16
  nodata = nodata :+ nodata
  toCDouble = fromIntegral . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Int32) where
  dataType _ = GDT_CInt32
  nodata = nodata :+ nodata
  toCDouble = fromIntegral . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Float) where
  dataType _ = GDT_CFloat32
  nodata = nodata :+ nodata
  toCDouble = realToFrac . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Complex Double) where
  dataType _ = GDT_CFloat64
  nodata = nodata :+ nodata
  toCDouble = realToFrac . realPart
  fromCDouble d = fromCDouble d :+ fromCDouble d
  fillBand (r :+ i) = fillRaster (toCDouble r) (toCDouble i)
  {-# INLINE fillBand #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}
#endif
