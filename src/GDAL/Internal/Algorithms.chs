{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module GDAL.Internal.Algorithms (
    GenImgProjTransformer
  , GenImgProjTransformer2
  , GenImgProjTransformer3
  , SomeTransformer (..)
  , HasTransformer (..)
  , TransformerFunPtr (..)
  , HasSrcSrs (..)
  , HasDstSrs (..)
  , HasSrcGt (..)
  , HasDstGt (..)
  , HasUseGCP (..)
  , HasOrder (..)
  , HasMaxError (..)
  , HasBands (..)
  , GDALAlgorithmException(..)

  , Contour (..)

  , GridPoint (..)
  , GridAlgorithm (..)
  , GridInverseDistanceToAPower (..)
  , GridMovingAverage (..)
  , GridNearestNeighbor (..)
  , GridDataMetrics (..)
  , MetricType (..)

  , GridAlgorithmEnum (..)

  , RasterizeSettings
  , rasterizeLayersBuf
  , rasterizeLayers
  , rasterizeGeometries
  , createGrid
  , createGridIO
  , computeProximity
  , contourGenerateVector
  , contourGenerateVectorIO

  , withTransformerAndArg
  , gipt
  , gipt2
  , gipt3
  , suggestedWarpOutput
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
import Control.Monad (liftM, when, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(..))
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import Lens.Micro

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
import GDAL.Internal.CPLConv (withConfigOption)
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.CPLProgress#}
{#import GDAL.Internal.OGR#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"
#include "contourwriter.h"
#include "suggestedwarpoutput.h"

data GDALAlgorithmException
  = NullTransformer !Text
  | GeomValueListLengthMismatch
  deriving (Typeable, Show, Eq)

instance Exception GDALAlgorithmException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

class Transformer t where
  transformerFun         :: t -> TransformerFun t
  createTransformerArg   :: t -> Maybe (Dataset s a m) -> IO (Ptr t)
  destroyTransformerArg  :: Ptr t -> IO ()
  setGeotransform        :: Geotransform -> Ptr t -> IO ()

  destroyTransformerArg p =
    when (p/=nullPtr) ({# call unsafe GDALDestroyTransformer as ^#} (castPtr p))

data SomeTransformer
  = forall t. Transformer t => SomeTransformer t

class HasTransformer o t | o -> t where
  transformer :: Lens' o t

class HasSrcSrs o a | o -> a where
  srcSrs :: Lens' o a

class HasDstSrs o a | o -> a where
  dstSrs :: Lens' o a

class HasSrcGt o a | o -> a where
  srcGt :: Lens' o a

class HasDstGt o a | o -> a where
  dstGt :: Lens' o a

class HasUseGCP o a | o -> a where
  useGCP :: Lens' o a

class HasOrder o a | o -> a where
  order :: Lens' o a

class HasMaxError o t | o -> t where
  maxError :: Lens' o t


withTransformerAndArg
  :: Maybe SomeTransformer
  -> Maybe (Dataset s a t)
  -> Maybe Geotransform
  -> (TransformerFunPtr -> Ptr () -> IO c)
  -> IO c
withTransformerAndArg Nothing _ _ act = act nullFunPtr nullPtr
withTransformerAndArg (Just (SomeTransformer t)) sDs mGt act =
  mask $ \restore -> do
    arg <- createTransformerArg t sDs
    case mGt of
      Just gt -> setGeotransform gt arg
      Nothing -> return ()
    -- Assumes arg will be destroyed by whoever takes it if not errors occur
    restore (act (getTransformerFunPtr (transformerFun t)) (castPtr arg))
              `onException` destroyTransformerArg arg


newtype TransformerFun t
  = TransformerFun {getTransformerFunPtr :: TransformerFunPtr}

{#pointer GDALTransformerFunc as TransformerFunPtr #}



-- ############################################################################
-- GenImgProjTransformer
-- ############################################################################

data GenImgProjTransformer =
     GenImgProjTransformer {
      giptSrcSrs   :: Maybe SpatialReference
    , giptDstSrs   :: Maybe SpatialReference
    , giptUseGCP   :: Bool
    , giptMaxError :: Double
    , giptOrder    :: Int
  }

gipt :: GenImgProjTransformer
gipt = def

instance HasSrcSrs (GenImgProjTransformer) (Maybe SpatialReference) where
  srcSrs = lens giptSrcSrs (\o a -> o {giptSrcSrs = a})

instance HasDstSrs (GenImgProjTransformer) (Maybe SpatialReference) where
  dstSrs = lens giptDstSrs (\o a -> o {giptDstSrs = a})

instance HasUseGCP (GenImgProjTransformer) Bool where
  useGCP = lens giptUseGCP (\o a -> o {giptUseGCP = a})

instance HasMaxError (GenImgProjTransformer) Double where
  maxError = lens giptMaxError (\o a -> o {giptMaxError = a})

instance HasOrder (GenImgProjTransformer) Int where
  order = lens giptOrder (\o a -> o {giptOrder = a})

instance Default (GenImgProjTransformer) where
  def = GenImgProjTransformer {
          giptSrcSrs   = Nothing
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
  createTransformerArg GenImgProjTransformer{..} srcDs =
    liftM castPtr $
    checkCreateTransformer "GenImgProjTransformer" $
    withMaybeSRAsCString giptSrcSrs $ \sSr ->
    withMaybeSRAsCString giptDstSrs $ \dSr ->
      {#call unsafe CreateGenImgProjTransformer as ^#}
        (maybe nullDatasetH unDataset srcDs)
        sSr
        nullDatasetH
        dSr
        (fromBool giptUseGCP)
        (realToFrac giptMaxError)
        (fromIntegral giptOrder)

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: TransformerFun GenImgProjTransformer


-- ############################################################################
-- GenImgProjTransformer2
-- ############################################################################

data GenImgProjTransformer2 =
     GenImgProjTransformer2 {
      gipt2Options  :: OptionList
  }

instance HasOptions (GenImgProjTransformer2) OptionList where
  options = lens gipt2Options (\o a -> o {gipt2Options = a})

gipt2 :: GenImgProjTransformer2
gipt2 = def

instance Default (GenImgProjTransformer2) where
  def = GenImgProjTransformer2 {
          gipt2Options = []
        }

instance Transformer GenImgProjTransformer2 where
  transformerFun _ = c_GDALGenImgProjTransform2
  setGeotransform = setGenImgProjTransfomerGeotransform
  createTransformerArg GenImgProjTransformer2{..} srcDs =
    liftM castPtr $
    checkCreateTransformer "GenImgProjTransformer2" $
    withOptionList gipt2Options $ \opts ->
      {#call unsafe CreateGenImgProjTransformer2 as ^#}
        (maybe nullDatasetH unDataset srcDs)
        nullDatasetH
        opts

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform2 :: TransformerFun GenImgProjTransformer2

-- ############################################################################
-- GenImgProjTransformer3
-- ############################################################################

data GenImgProjTransformer3 =
     GenImgProjTransformer3 {
      gipt3SrcSrs :: Maybe SpatialReference
    , gipt3DstSrs :: Maybe SpatialReference
    , gipt3SrcGt  :: Maybe Geotransform
    , gipt3DstGt  :: Maybe Geotransform
  }

gipt3 :: GenImgProjTransformer3
gipt3 = def

instance HasSrcSrs (GenImgProjTransformer3) (Maybe SpatialReference) where
  srcSrs = lens gipt3SrcSrs (\o a -> o {gipt3SrcSrs = a})

instance HasDstSrs (GenImgProjTransformer3) (Maybe SpatialReference) where
  dstSrs = lens gipt3DstSrs (\o a -> o {gipt3DstSrs = a})

instance HasSrcGt (GenImgProjTransformer3) (Maybe Geotransform) where
  srcGt = lens gipt3SrcGt (\o a -> o {gipt3SrcGt = a})

instance HasDstGt (GenImgProjTransformer3) (Maybe Geotransform) where
  dstGt = lens gipt3DstGt (\o a -> o {gipt3DstGt = a})

instance Default (GenImgProjTransformer3) where
  def = GenImgProjTransformer3 {
          gipt3SrcSrs = Nothing
        , gipt3DstSrs = Nothing
        , gipt3SrcGt  = Nothing
        , gipt3DstGt  = Nothing
        }

instance Transformer GenImgProjTransformer3 where
  transformerFun _ = c_GDALGenImgProjTransform3
  setGeotransform = setGenImgProjTransfomerGeotransform
  createTransformerArg GenImgProjTransformer3{..} srcDs = do
    sSrs <- case (gipt3SrcSrs, srcDs) of
              (Nothing, Just ds) -> datasetProjection ds
              _                  -> return gipt3SrcSrs
    sGt <- case (gipt3SrcGt, srcDs) of
              (Nothing, Just ds) -> datasetGeotransform ds
              _                  -> return gipt3SrcGt
    liftM castPtr $
      checkCreateTransformer "GenImgProjTransformer3" $
      withMaybeSRAsCString sSrs $ \sSrsPtr ->
      withMaybeSRAsCString gipt3DstSrs $ \dSrsPtr ->
      withMaybeGeotransformPtr sGt $ \sGtPtr ->
      withMaybeGeotransformPtr gipt3DstGt $ \dGtPtr ->
        {#call unsafe CreateGenImgProjTransformer3 as ^#}
          sSrsPtr (castPtr sGtPtr) dSrsPtr (castPtr dGtPtr)

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
  c_GDALGenImgProjTransform3 :: TransformerFun GenImgProjTransformer3

-- ############################################################################
-- RasterizeSettings
-- ############################################################################

data RasterizeSettings =
  RasterizeSettings {
    rsTransformer :: Maybe SomeTransformer
  , rsOptions     :: OptionList
  , rsProgressFun :: Maybe ProgressFun
  , rsBands       :: [Int]
  }

instance Default (RasterizeSettings) where
  def = RasterizeSettings def def def [1]

instance HasOptions (RasterizeSettings) OptionList where
  options = lens rsOptions (\a b -> a {rsOptions = b})

instance HasTransformer (RasterizeSettings) (Maybe SomeTransformer)
  where
    transformer = lens rsTransformer (\a b -> a {rsTransformer = b})

instance HasProgressFun (RasterizeSettings) (Maybe ProgressFun) where
  progressFun = lens rsProgressFun (\a b -> a {rsProgressFun = b})

class HasBands o a | o -> a where
  bands :: Lens' o a

instance HasBands (RasterizeSettings) [Int] where
  bands = lens rsBands (\a b -> a {rsBands = b})

-- ############################################################################
-- GDALRasterizeLayersBuf
-- ############################################################################


rasterizeLayersBuf
  :: forall s l b a. GDALType a
  => [ROLayer s l b]
  -> a
  -> Either a Text
  -> SpatialReference
  -> Size
  -> Geotransform
  -> RasterizeSettings
  -> GDAL s (U.Vector (Value a))
rasterizeLayersBuf layers nodataValue burnValueOrAttr srs size gt cfg =
  liftIO $
  withProgressFun "rasterizeLayersBuf" (cfg^.progressFun) $ \pFun ->
  withArrayLen (map unLayer layers) $ \len lPtrPtr ->
  withMaybeSRAsCString (Just srs) $ \srsPtr ->
  withOptionList options' $ \opts ->
  withTransformerAndArg (cfg^.transformer) Nothing (Just gt) $ \trans tArg ->
  with gt $ \pGt -> do
    vec <- GM.replicate (sizeLen size) nodataValue
    Stm.unsafeWith vec $ \vecPtr ->
      checkCPLError "RasterizeLayersBuf" $
      {#call GDALRasterizeLayersBuf as ^#}
        (castPtr vecPtr) nx ny (fromEnumC dt) pxSpace 0 (fromIntegral len)
        lPtrPtr srsPtr (castPtr pGt) trans
        tArg bValue opts pFun nullPtr
    liftM (mkValueUVector nodataValue) (G.unsafeFreeze vec)
  where
    options' = case burnValueOrAttr of
      Left _  -> cfg^.options
      Right a -> ("attribute",a):(cfg^.options)
    dt = hsDataType (Proxy :: Proxy a)
    bValue    = toCDouble (either id (const nodataValue) burnValueOrAttr)
    nx :+: ny  = fmap fromIntegral size
    pxSpace = fromIntegral (sizeOf (undefined :: a))


-- ############################################################################
-- GDALRasterizeLayers
-- ############################################################################


rasterizeLayers
  :: forall s l b a. GDALType a
  => Either [(ROLayer s l b, a)] ([ROLayer s l b], Text)
  -> RWDataset s a
  -> RasterizeSettings
  -> GDAL s ()
rasterizeLayers eLayers ds cfg =
  liftIO $
  withProgressFun "rasterizeLayers" (cfg^.progressFun) $ \pFun ->
  withTransformerAndArg (cfg^.transformer) Nothing Nothing $ \trans tArg ->
  withArrayLen (cfg^.bands) $ \nBands bListPtr ->
    case eLayers of
      Left (unzip -> (layers, values)) ->
        withArrayLen (map unLayer layers) $ \nLayers lListPtr ->
        withArrayLen (map toCDouble values) $ \_ valPtr ->
        withOptionList (cfg^.options) $ \opts ->
        checkCPLError "RasterizeLayers" $
        {#call GDALRasterizeLayers as ^#}
          (unDataset ds)
          (fromIntegral nBands)
          (castPtr bListPtr)
          (fromIntegral nLayers)
          (castPtr lListPtr)
          trans
          tArg
          valPtr
          opts
          pFun
          nullPtr
      Right (layers, attr) ->
        withArrayLen (map unLayer layers) $ \nLayers lListPtr ->
        withOptionList (("attribute",attr):(cfg^.options)) $ \opts ->
        checkCPLError "RasterizeLayers" $
        {#call GDALRasterizeLayers as ^#}
          (unDataset ds)
          (fromIntegral nBands)
          (castPtr bListPtr)
          (fromIntegral nLayers)
          (castPtr lListPtr)
          trans
          tArg
          nullPtr
          opts
          pFun
          nullPtr

-- ############################################################################
-- GDALRasterizeGeometries
-- ############################################################################


rasterizeGeometries
  :: forall s a. GDALType a
  => [(Geometry, [a])]
  -> RWDataset s a
  -> RasterizeSettings
  -> GDAL s ()
rasterizeGeometries geomValues ds cfg = do
  values <- liftM concat $ forM geomValues $ \(_,vals) ->
    if length vals == length (cfg^.bands)
      then return vals
      else throwBindingException GeomValueListLengthMismatch
  liftIO $
    withProgressFun "rasterizeGeometries" (cfg^.progressFun) $ \pFun ->
    withTransformerAndArg (cfg^.transformer) Nothing Nothing $ \trans tArg ->
    withArrayLen (cfg^.bands) $ \nBands bListPtr ->
    withGeometries (map fst geomValues) $ \pGeoms ->
    withArrayLen pGeoms $ \nGeoms gListPtr ->
    withArrayLen (map toCDouble values) $ \_ valPtr ->
    withOptionList (cfg^.options) $ \opts ->
      checkCPLError "RasterizeGeometries" $
      {#call GDALRasterizeGeometries as ^#}
        (unDataset ds)
        (fromIntegral nBands)
        (castPtr bListPtr)
        (fromIntegral nGeoms)
        (castPtr gListPtr)
        trans
        tArg
        valPtr
        opts
        pFun
        nullPtr

-- ############################################################################
-- GDALCreateGrid
-- ############################################################################

{# enum GDALGridAlgorithm as GridAlgorithmEnum {}
    deriving (Eq,Read,Show,Bounded) #}


class (Storable a, Typeable a, Default a, Show a) => GridAlgorithm a where
  gridAlgorithm :: a -> GridAlgorithmEnum
  setNodata     :: Ptr a -> CDouble -> IO ()

createGridIO
  :: (GDALType (HsType d), GridAlgorithm opts, IsComplex d ~ 'False)
  => DataType d
  -> opts
  -> HsType d
  -> Maybe ProgressFun
  -> St.Vector (GridPoint (HsType d))
  -> EnvelopeReal
  -> Size
  -> IO (U.Vector (Value (HsType d)))
createGridIO dt options noDataVal progressFun points envelope size =
  --FIXME: Need to set this to 1 or else it will hang with GDAL 2.1
  withConfigOption "GDAL_NUM_THREADS" (Just "1") $
  withProgressFun "createGridIO" progressFun $ \pFun ->
  with options $ \opts -> do
    setNodata opts (toCDouble noDataVal)
    xs <- G.unsafeThaw (St.unsafeCast (St.map (pFst . gpXY) points))
    ys <- G.unsafeThaw (St.unsafeCast (St.map (pSnd . gpXY) points))
    zs <- G.unsafeThaw (St.map (toCDouble . gpZ) points)
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
        (fromEnumC dt)
        (castPtr pOut)
        pFun
        nullPtr
    liftM (mkValueUVector noDataVal) (G.unsafeFreeze out)
  where
    nx :+: ny                       = fmap fromIntegral size
    Envelope (x0 :+: y0) (x1 :+: y1) = fmap realToFrac envelope


createGrid
  :: (GDALType (HsType d), GridAlgorithm opts, IsComplex d ~ 'False)
  => DataType d
  -> opts
  -> HsType d
  -> St.Vector (GridPoint (HsType d))
  -> EnvelopeReal
  -> Size
  -> Either GDALException (U.Vector (Value (HsType d)))
createGrid dt options noDataVal points envelope =
  unsafePerformIO .
  try .
  createGridIO dt options noDataVal Nothing points envelope


data GridPoint a =
  GP {
    gpXY :: {-# UNPACK #-} !(Pair Double)
  , gpZ  ::                !a
  } deriving (Eq, Show, Read)

instance Storable a => Storable (GridPoint a) where
  sizeOf _ = sizeOf (undefined::Pair Double) + sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (GP xy z) = poke ptr' xy >> poke (castPtr (ptr' `advancePtr` 1)) z
    where ptr' = castPtr ptr :: Ptr (Pair Double)
  {-# INLINE poke #-}
  peek ptr = GP <$> peek ptr'
                <*> peek (castPtr (ptr' `advancePtr` 1))
    where ptr' = castPtr ptr :: Ptr (Pair Double)
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
  , cPoints :: {-# UNPACK #-} !(St.Vector (Pair Double))
  } deriving (Eq, Show)

{#pointer ContourList #}
{#pointer *Point->PairDouble #}

type PairDouble = Pair Double

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
contourGenerateVectorIO interval base nodataVal (nx :+: ny) vector =
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
  c_destroyPoints :: FunPtr (Ptr PairDouble -> IO ())

suggestedWarpOutput
  :: SomeTransformer -> Size -> Either GDALException (Geotransform, Size)
suggestedWarpOutput trans (fmap fromIntegral -> xSize :+: ySize) =
  unsafePerformIO $
  alloca $ \gt ->
  alloca $ \nPixels ->
  alloca $ \nLines ->
  try $
  withTransformerAndArg (Just trans) Nothing Nothing $ \t tArg -> do
    checkCPLError "suggestedWarpOutput" $
      {#call unsafe hs_gdal_suggested_warp_output#}
        xSize ySize t tArg (castPtr gt) nPixels nLines
    (,) <$> peek gt <*> ((:+:) <$> (fromIntegral <$> peek nPixels)
                               <*> (fromIntegral <$> peek nLines))
