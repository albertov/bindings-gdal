{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}

module GDAL.Internal.Algorithms (
    GenImgProjTransformer (..)
  , GenImgProjTransformer2 (..)
  , GenImgProjTransformer3 (..)
  , SomeTransformer (..)

  , Contour (..)

  , GridPoint (..)
  , GridAlgorithm (..)
  , GridInverseDistanceToAPower (..)
  , GridMovingAverage (..)
  , GridNearestNeighbor (..)
  , GridDataMetrics (..)
  , MetricType (..)

  , GridAlgorithmEnum (..)

  , rasterizeLayersBuf
  , createGrid
  , createGridIO
  , computeProximity
  , contourGenerateVector
  , contourGenerateVectorIO

  , withTransformerAndArg
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Catch (
    Exception(..)
  , bracket
  , mask
  , mask_
  , onException
  , try
  )
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CChar(..))
import Foreign.Marshal.Utils (fromBool)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Ptr (
    Ptr
  , FunPtr
  , nullPtr
  , castPtr
  , nullFunPtr
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen, advancePtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.Types
import GDAL.Internal.Types.Value
import GDAL.Internal.DataType
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.CPLProgress#}
{#import GDAL.Internal.OGR#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"
#include "contourwriter.h"

data GDALAlgorithmException
  = NullTransformer !Text
  deriving (Typeable, Show, Eq)

instance Exception GDALAlgorithmException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

class Transformer t where
  transformerFun         :: t s -> TransformerFun t s
  createTransformerArg   :: t s -> IO (Ptr (t s))
  destroyTransformerArg  :: Ptr (t s) -> IO ()
  setGeotransform        :: Geotransform -> Ptr (t s) -> IO ()

  destroyTransformerArg p =
    when (p/=nullPtr) ({# call unsafe GDALDestroyTransformer as ^#} (castPtr p))

data SomeTransformer s
  = forall t. Transformer t => SomeTransformer (t s)
  | DefaultTransformer

instance Default (SomeTransformer s) where
  def = DefaultTransformer

withTransformerAndArg
  :: SomeTransformer s
  -> Maybe Geotransform
  -> (TransformerFunPtr -> Ptr () -> IO c)
  -> IO c
withTransformerAndArg DefaultTransformer _ act  = act nullFunPtr nullPtr
withTransformerAndArg (SomeTransformer t) mGt act =
  mask $ \restore -> do
    arg <- createTransformerArg t
    case mGt of
      Just gt -> setGeotransform gt arg
      Nothing -> return ()
    -- Assumes arg will be destroyed by whoever takes it if not errors occur
    restore (act (getTransformerFunPtr (transformerFun t)) (castPtr arg))
              `onException` destroyTransformerArg arg


newtype TransformerFun (t :: * -> *) s
  = TransformerFun {getTransformerFunPtr :: TransformerFunPtr}

{#pointer GDALTransformerFunc as TransformerFunPtr #}



-- ############################################################################
-- GenImgProjTransformer
-- ############################################################################

data GenImgProjTransformer s =
     GenImgProjTransformer {
      giptSrcDs    :: Maybe (RODataset s)
    , giptDstDs    :: Maybe (RWDataset s)
    , giptSrcSrs   :: Maybe SpatialReference
    , giptDstSrs   :: Maybe SpatialReference
    , giptUseGCP   :: Bool
    , giptMaxError :: Double
    , giptOrder    :: Int
  }

instance Default (GenImgProjTransformer s) where
  def = GenImgProjTransformer {
          giptSrcDs    = Nothing
        , giptDstDs    = Nothing
        , giptSrcSrs   = Nothing
        , giptDstSrs   = Nothing
        , giptUseGCP   = True
        , giptMaxError = 1
        , giptOrder    = 0
        }

checkCreateTransformer :: Text -> IO (Ptr ()) -> IO (Ptr ())
checkCreateTransformer msg = checkGDALCall checkit
  where
    checkit e p
      | p==nullPtr = Just (NullTransformer (maybe msg gdalErrMsg e))
      | otherwise  = Nothing

instance Transformer GenImgProjTransformer where
  transformerFun _ = c_GDALGenImgProjTransform
  setGeotransform = setGenImgProjTransfomerGeotransform
  createTransformerArg GenImgProjTransformer{..} =
    liftM castPtr $
    checkCreateTransformer "GenImgProjTransformer" $
    withMaybeSRAsCString giptSrcSrs $ \sSr ->
    withMaybeSRAsCString giptDstSrs $ \dSr ->
      {#call unsafe CreateGenImgProjTransformer as ^#}
        (maybe nullDatasetH unDataset giptSrcDs)
        sSr
        (maybe nullDatasetH unDataset giptDstDs)
        dSr
        (fromBool giptUseGCP)
        (realToFrac giptMaxError)
        (fromIntegral giptOrder)

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: TransformerFun GenImgProjTransformer s


-- ############################################################################
-- GenImgProjTransformer2
-- ############################################################################

data GenImgProjTransformer2 s =
     GenImgProjTransformer2 {
      gipt2SrcDs    :: Maybe (RODataset s)
    , gipt2DstDs    :: Maybe (RWDataset s)
    , gipt2Options  :: OptionList
  }

instance Default (GenImgProjTransformer2 s) where
  def = GenImgProjTransformer2 {
          gipt2SrcDs   = Nothing
        , gipt2DstDs   = Nothing
        , gipt2Options = []
        }

instance Transformer GenImgProjTransformer2 where
  transformerFun _ = c_GDALGenImgProjTransform2
  setGeotransform = setGenImgProjTransfomerGeotransform
  createTransformerArg GenImgProjTransformer2{..} =
    liftM castPtr $
    checkCreateTransformer "GenImgProjTransformer2" $
    withOptionList gipt2Options $ \opts ->
      {#call unsafe CreateGenImgProjTransformer2 as ^#}
        (maybe nullDatasetH unDataset gipt2SrcDs)
        (maybe nullDatasetH unDataset gipt2DstDs)
        opts

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform2 :: TransformerFun GenImgProjTransformer2 s

-- ############################################################################
-- GenImgProjTransformer3
-- ############################################################################

data GenImgProjTransformer3 s =
     GenImgProjTransformer3 {
      gipt3SrcSrs :: Maybe SpatialReference
    , gipt3DstSrs :: Maybe SpatialReference
    , gipt3SrcGt  :: Maybe Geotransform
    , gipt3DstGt  :: Maybe Geotransform
  }

instance Default (GenImgProjTransformer3 s) where
  def = GenImgProjTransformer3 {
          gipt3SrcSrs = Nothing
        , gipt3DstSrs = Nothing
        , gipt3SrcGt  = Nothing
        , gipt3DstGt  = Nothing
        }

instance Transformer GenImgProjTransformer3 where
  transformerFun _ = c_GDALGenImgProjTransform3
  setGeotransform = setGenImgProjTransfomerGeotransform
  createTransformerArg GenImgProjTransformer3{..} =
    liftM castPtr $
    checkCreateTransformer "GenImgProjTransformer3" $
    withMaybeSRAsCString gipt3SrcSrs $ \sSr ->
    withMaybeSRAsCString gipt3DstSrs $ \dSr ->
    withMaybeGeotransformPtr gipt3SrcGt $ \sGt ->
    withMaybeGeotransformPtr gipt3DstGt $ \dGt ->
      {#call unsafe CreateGenImgProjTransformer3 as ^#}
        sSr (castPtr sGt) dSr (castPtr dGt)

setGenImgProjTransfomerGeotransform :: Geotransform -> Ptr a -> IO ()
setGenImgProjTransfomerGeotransform geotransform pArg =
  with geotransform $ \gt ->
    {#call unsafe GDALSetGenImgProjTransformerDstGeoTransform as ^#}
    (castPtr pArg) (castPtr gt)


withMaybeGeotransformPtr
  :: Maybe Geotransform -> (Ptr Geotransform -> IO a) -> IO a
withMaybeGeotransformPtr Nothing   f = f nullPtr
withMaybeGeotransformPtr (Just g) f = alloca $ \gp -> poke gp g >> f gp

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform3 :: TransformerFun GenImgProjTransformer3 s

-- ############################################################################
-- GDALRasterizeLayersBuf
-- ############################################################################

rasterizeLayersBuf
  :: forall s l b a. GDALType a
  => GDAL s [ROLayer s l b]
  -> SomeTransformer s
  -> a
  -> a
  -> OptionList
  -> Maybe ProgressFun
  -> SpatialReference
  -> Size
  -> Geotransform
  -> OGR s l (U.Vector (Value a))
rasterizeLayersBuf getLayers mTransformer nodataValue
                   burnValue options progressFun
                   srs size geotransform =
  bracket (liftOGR getLayers) (mapM_ closeLayer) $ \layers ->
  liftIO $
  withProgressFun "rasterizeLayersBuf" progressFun $ \pFun ->
  withArrayLen (map unLayer layers) $ \len lPtrPtr ->
  withMaybeSRAsCString (Just srs) $ \srsPtr ->
  withOptionList options $ \opts ->
  withTransformerAndArg mTransformer (Just geotransform) $ \trans tArg ->
  with geotransform $ \gt -> do
    vec <- GM.replicate (sizeLen size) nodataValue
    Stm.unsafeWith vec $ \vecPtr ->
      checkCPLError "RasterizeLayersBuf" $
      {#call GDALRasterizeLayersBuf as ^#}
        (castPtr vecPtr) nx ny (fromEnumC dt) 0 0 (fromIntegral len)
        lPtrPtr srsPtr (castPtr gt) trans
        tArg bValue opts pFun nullPtr
    liftM (mkValueUVector nodataValue) (G.unsafeFreeze vec)
  where
    dt = dataType (undefined :: a)
    bValue    = toCDouble burnValue
    XY nx ny  = fmap fromIntegral size


-- ############################################################################
-- GDALCreateGrid
-- ############################################################################

{# enum GDALGridAlgorithm as GridAlgorithmEnum {}
    deriving (Eq,Read,Show,Bounded) #}


class (Storable a, Typeable a, Default a, Show a) => GridAlgorithm a where
  gridAlgorithm :: a -> GridAlgorithmEnum
  setNodata     :: Ptr a -> CDouble -> IO ()

createGridIO
  :: GridAlgorithm opts
  => opts
  -> Double
  -> Maybe ProgressFun
  -> St.Vector GridPoint
  -> EnvelopeReal
  -> Size
  -> IO (U.Vector (Value Double))
createGridIO options noDataVal progressFun points envelope size =
  withProgressFun "createGridIO" progressFun $ \pFun ->
  withErrorHandler $
  with options $ \opts -> do
    setNodata opts (realToFrac noDataVal)
    xs <- G.unsafeThaw (St.unsafeCast (St.map (px . gpXY) points))
    ys <- G.unsafeThaw (St.unsafeCast (St.map (py . gpXY) points))
    zs <- G.unsafeThaw (St.unsafeCast (St.map gpZ         points))
    out <- GM.unsafeNew (sizeLen size)
    checkCPLError "GDALGridCreate" $
      Stm.unsafeWith xs $ \pXs ->
      Stm.unsafeWith ys $ \pYs ->
      Stm.unsafeWith zs $ \pZs ->
      Stm.unsafeWith out $ \pOut ->
      {#call GDALGridCreate as ^#}
        (fromEnumC (gridAlgorithm options))
        (castPtr opts)
        (fromIntegral (St.length points))
        pXs
        pYs
        pZs
        x0
        x1
        y0
        y1
        nx
        ny
        (fromEnumC GDT_Float64)
        (castPtr pOut)
        pFun
        nullPtr
    liftM (mkValueUVector noDataVal) (G.unsafeFreeze out)
  where
    XY nx ny                       = fmap fromIntegral size
    Envelope (XY x0 y0) (XY x1 y1) = fmap realToFrac envelope
{-# INLINE createGridIO #-}


createGrid
  :: GridAlgorithm opts
  => opts
  -> Double
  -> St.Vector GridPoint
  -> EnvelopeReal
  -> Size
  -> Either GDALException (U.Vector (Value Double))
createGrid options noDataVal points envelope =
  unsafePerformIO .
  try .
  createGridIO options noDataVal Nothing points envelope
{-# INLINE createGrid #-}


data GridPoint =
  GP {
    gpXY :: {-# UNPACK #-} !(XY Double)
  , gpZ  :: {-# UNPACK #-} !Double
  } deriving (Eq, Show, Read)

instance Storable GridPoint where
  sizeOf _ = sizeOf (undefined::XY Double) + sizeOf (undefined::Double)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::Double)
  {-# INLINE alignment #-}
  poke ptr (GP xy z) = poke ptr' xy >> poke (castPtr (ptr' `advancePtr` 1)) z
    where ptr' = castPtr ptr :: Ptr (XY Double)
  {-# INLINE poke #-}
  peek ptr = GP <$> peek ptr'
                <*> peek (castPtr (ptr' `advancePtr` 1))
    where ptr' = castPtr ptr :: Ptr (XY Double)
  {-# INLINE peek #-}

-- ############################################################################
-- GridInverseDistanceToAPower
-- ############################################################################

data GridInverseDistanceToAPower =
  GridInverseDistanceToAPower {
    idpPower           :: !Double
  , idpSmoothing       :: !Double
  , idpAnisotropyRatio :: !Double
  , idpAnisotropyAngle :: !Double
  , idpRadius1         :: !Double
  , idpRadius2         :: !Double
  , idpAngle           :: !Double
  , idpMinPoints       :: !Int
  , idpMaxPoints       :: !Int
  } deriving (Eq, Show, Typeable)

instance GridAlgorithm GridInverseDistanceToAPower where
  gridAlgorithm = const GGA_InverseDistanceToAPower
  setNodata     = {#set GridInverseDistanceToAPowerOptions->dfNoDataValue#}

instance Default GridInverseDistanceToAPower where
  def = GridInverseDistanceToAPower {
          idpPower           = 2
        , idpSmoothing       = 0
        , idpAnisotropyRatio = 0
        , idpAnisotropyAngle = 0
        , idpRadius1         = 0
        , idpRadius2         = 0
        , idpAngle           = 0
        , idpMinPoints       = 0
        , idpMaxPoints       = 0
        }

instance Storable GridInverseDistanceToAPower where
  sizeOf _    = {#sizeof GridInverseDistanceToAPowerOptions#}
  alignment _ = {#alignof GridInverseDistanceToAPowerOptions#}
  peek _      = error "GridInverseDistanceToAPower: peek not implemented"
  poke p GridInverseDistanceToAPower{..}= do
    {#set GridInverseDistanceToAPowerOptions->dfPower#} p
      (realToFrac idpPower)
    {#set GridInverseDistanceToAPowerOptions->dfSmoothing#} p
      (realToFrac idpSmoothing)
    {#set GridInverseDistanceToAPowerOptions->dfAnisotropyRatio#} p
      (realToFrac idpAnisotropyRatio)
    {#set GridInverseDistanceToAPowerOptions->dfAnisotropyAngle#} p
      (realToFrac idpAnisotropyAngle)
    {#set GridInverseDistanceToAPowerOptions->dfRadius1#} p
      (realToFrac idpRadius1)
    {#set GridInverseDistanceToAPowerOptions->dfRadius2#} p
      (realToFrac idpRadius2)
    {#set GridInverseDistanceToAPowerOptions->dfAngle#} p
      (realToFrac idpAngle)
    {#set GridInverseDistanceToAPowerOptions->nMinPoints#} p
      (fromIntegral idpMinPoints)
    {#set GridInverseDistanceToAPowerOptions->nMaxPoints#} p
      (fromIntegral idpMaxPoints)

-- ############################################################################
-- GridMovingAverage
-- ############################################################################

data GridMovingAverage =
  GridMovingAverage {
    maRadius1   :: !Double
  , maRadius2   :: !Double
  , maAngle     :: !Double
  , maMinPoints :: !Int
  } deriving (Eq, Show, Typeable)

instance GridAlgorithm GridMovingAverage where
  gridAlgorithm = const GGA_MovingAverage
  setNodata     = {#set GridMovingAverageOptions->dfNoDataValue#}

instance Default GridMovingAverage where
  def = GridMovingAverage {
          maRadius1   = 0
        , maRadius2   = 0
        , maAngle     = 0
        , maMinPoints = 0
        }

instance Storable GridMovingAverage where
  sizeOf _    = {#sizeof GridMovingAverageOptions#}
  alignment _ = {#alignof GridMovingAverageOptions#}
  peek _      = error "GridMovingAverage: peek not implemented"
  poke p GridMovingAverage{..}= do
    {#set GridMovingAverageOptions->dfRadius1#} p
      (realToFrac maRadius1)
    {#set GridMovingAverageOptions->dfRadius2#} p
      (realToFrac maRadius2)
    {#set GridMovingAverageOptions->dfAngle#} p
      (realToFrac maAngle)
    {#set GridMovingAverageOptions->nMinPoints#} p
      (fromIntegral maMinPoints)

-- ############################################################################
-- GridNearestNeighbor
-- ############################################################################

data GridNearestNeighbor =
  GridNearestNeighbor {
    nnRadius1   :: !Double
  , nnRadius2   :: !Double
  , nnAngle     :: !Double
  } deriving (Eq, Show, Typeable)

instance GridAlgorithm GridNearestNeighbor where
  gridAlgorithm = const GGA_NearestNeighbor
  setNodata     = {#set GridNearestNeighborOptions->dfNoDataValue#}

instance Default GridNearestNeighbor where
  def = GridNearestNeighbor {
          nnRadius1   = 0
        , nnRadius2   = 0
        , nnAngle     = 0
        }

instance Storable GridNearestNeighbor where
  sizeOf _    = {#sizeof GridNearestNeighborOptions#}
  alignment _ = {#alignof GridNearestNeighborOptions#}
  peek _      = error "GridNearestNeighbor: peek not implemented"
  poke p GridNearestNeighbor{..}= do
    {#set GridNearestNeighborOptions->dfRadius1#} p
      (realToFrac nnRadius1)
    {#set GridNearestNeighborOptions->dfRadius2#} p
      (realToFrac nnRadius2)
    {#set GridNearestNeighborOptions->dfAngle#} p
      (realToFrac nnAngle)

-- ############################################################################
-- GridDataMetrics
-- ############################################################################

data MetricType
  = MetricMinimum
  | MetricMaximum
  | MetricRange
  | MetricCount
  | MetricAverageDistance
  | MetricAverageDistancePts
  deriving (Eq, Read, Show, Bounded)


instance Default MetricType where
  def = MetricCount


data GridDataMetrics =
  GridDataMetrics {
    dmRadius1   :: !Double
  , dmRadius2   :: !Double
  , dmAngle     :: !Double
  , dmMinPoints :: !Int
  , dmType      :: !MetricType
  } deriving (Eq, Show, Typeable)

instance GridAlgorithm GridDataMetrics where
  gridAlgorithm = mtToGGA . dmType
    where
      mtToGGA MetricMinimum            = GGA_MetricMinimum
      mtToGGA MetricMaximum            = GGA_MetricMaximum
      mtToGGA MetricRange              = GGA_MetricRange
      mtToGGA MetricCount              = GGA_MetricCount
      mtToGGA MetricAverageDistance    = GGA_MetricAverageDistance
      mtToGGA MetricAverageDistancePts = GGA_MetricAverageDistancePts
  setNodata     = {#set GridDataMetricsOptions->dfNoDataValue#}

instance Default GridDataMetrics where
  def = GridDataMetrics {
          dmRadius1   = 0
        , dmRadius2   = 0
        , dmAngle     = 0
        , dmMinPoints = 0
        , dmType      = def
        }

instance Storable GridDataMetrics where
  sizeOf _    = {#sizeof GridDataMetricsOptions#}
  alignment _ = {#alignof GridDataMetricsOptions#}
  peek _      = error "GridDataMetrics: peek not implemented"
  poke p GridDataMetrics{..}= do
    {#set GridDataMetricsOptions->dfRadius1#} p
      (realToFrac dmRadius1)
    {#set GridDataMetricsOptions->dfRadius2#} p
      (realToFrac dmRadius2)
    {#set GridDataMetricsOptions->dfAngle#} p
      (realToFrac dmAngle)
    {#set GridDataMetricsOptions->nMinPoints#} p
      (fromIntegral dmMinPoints)

-- ############################################################################
-- ComputeProximity
-- ############################################################################

computeProximity
  :: Band s a t
  -> RWBand s a
  -> OptionList
  -> Maybe ProgressFun
  -> GDAL s ()
computeProximity srcBand prxBand options progressFun =
  liftIO $
  withOptionList options $ \opts ->
  withProgressFun "computeProximity" progressFun $ \pFun ->
  checkCPLError "computeProximity" $
  {#call GDALComputeProximity as ^#}
    (unBand srcBand)
    (unBand prxBand)
    opts
    pFun
    nullPtr

-- ############################################################################
-- contourVector
-- ############################################################################

data Contour =
  Contour {
    cLevel  :: {-# UNPACK #-} !Double
  , cPoints :: {-# UNPACK #-} !(St.Vector (XY Double))
  } deriving (Eq, Show)

{#pointer ContourList #}
{#pointer *Point->XYDouble #}

type XYDouble = XY Double

contourGenerateVectorIO
  :: Double
  -> Double
  -> Maybe Double
  -> Size
  -> (St.Vector Double)
  -> IO [Contour]
contourGenerateVectorIO _ _ _ size vector
  | St.length vector /= sizeLen size =
      throwBindingException (InvalidRasterSize size)
contourGenerateVectorIO interval base nodataVal (XY nx ny) vector =
  withErrorHandler $
  with nullPtr $ \pList ->
  bracket (alloc pList) (free pList) $ \generator -> do
    St.forM_ (St.enumFromStepN 0 nx ny) $ \offset ->
      St.unsafeWith (St.slice offset nx (St.unsafeCast vector)) $
      checkCPLError "FeedLine" . {#call unsafe GDAL_CG_FeedLine as ^#} generator
    mask_ (alloca (alloca . getContours [] pList))
  where
    alloc pList =
      {#call unsafe GDAL_CG_Create as ^#}
        (fromIntegral nx)
        (fromIntegral ny)
        (fromBool (isJust nodataVal))
        (maybe 0 realToFrac nodataVal)
        (realToFrac interval)
        (realToFrac base)
        c_contourWriter
        (castPtr pList)

    free pList gen = do
      {#call unsafe destroy_contours#} pList
      when (gen/=nullPtr) ({#call unsafe GDAL_CG_Destroy as ^#} gen)

    getContours acc pList pLen pLevel = do
      ps <- {#call unsafe pop_contour#} pList pLevel pLen
      if ps==nullPtr
        then return acc
        else do
          c <- Contour
                <$> liftM realToFrac (peek pLevel)
                <*> (St.unsafeFromForeignPtr0
                       <$> newForeignPtr c_destroyPoints ps
                       <*> liftM fromIntegral (peek pLen))
          getContours (c:acc) pList pLen pLevel

contourGenerateVector
  :: Double
  -> Double
  -> Maybe Double
  -> Size
  -> (St.Vector Double)
  -> Either GDALException [Contour]
contourGenerateVector interval base nodataVal size =
  unsafePerformIO . try . contourGenerateVectorIO interval base nodataVal size

type CContourWriter =
  CDouble -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr () -> IO CInt

foreign import ccall "contourwriter.h &hs_contour_writer"
  c_contourWriter :: FunPtr CContourWriter

foreign import ccall "contourwriter.h &destroy_points"
  c_destroyPoints :: FunPtr (Ptr XYDouble -> IO ())
