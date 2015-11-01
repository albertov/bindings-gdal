{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}

module GDAL.Internal.Algorithms (
    GenImgProjTransformer (..)
  , GenImgProjTransformer2 (..)
  , GenImgProjTransformer3 (..)
  , SomeTransformer (..)

  , GridAlgorithmOptions (..)
  , GridAlgorithm (..)
  , InverseDistanceToAPowerOptions (..)
  , GridPoint (..)

  , withTransformerAndArg

  , rasterizeLayersBuf
  , createGrid

  , createGridIO
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad.Catch (Exception(..), bracket, mask, onException, try)
import Control.Monad (liftM, mapM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(..))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CChar(..))
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, nullFunPtr, plusPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.Types
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.CPLProgress#}
{#import GDAL.Internal.OGR#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"

data GDALAlgorithmException
  = RasterizeStopped
  | CreateGridStopped
  | NullTransformer !Text
  deriving (Typeable, Show, Eq)

instance NFData GDALAlgorithmException where
  rnf a = a `seq` ()

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
  :: forall s l a b. GDALType a
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
  withProgressFun RasterizeStopped progressFun $ \pFun ->
  withArrayLen (map unLayer layers) $ \len lPtrPtr ->
  withMaybeSRAsCString (Just srs) $ \srsPtr ->
  withOptionList options $ \opts ->
  withTransformerAndArg mTransformer (Just geotransform) $ \trans tArg ->
  with geotransform $ \gt -> do
    vec <- Stm.replicate (sizeLen size) nodataValue
    Stm.unsafeWith vec $ \vecPtr ->
      checkCPLError "RasterizeLayersBuf" $
      {#call GDALRasterizeLayersBuf as ^#}
        (castPtr vecPtr) nx ny dt 0 0 (fromIntegral len)
        lPtrPtr srsPtr (castPtr gt) trans
        tArg bValue opts pFun nullPtr
    liftM (stToUValue . St.map toValue) (St.unsafeFreeze vec)
  where
    toValue v = if toCDouble v == ndValue then NoData else Value v
    dt        = fromEnumC (dataType (Proxy :: Proxy a))
    bValue    = toCDouble burnValue
    ndValue   = toCDouble nodataValue
    XY nx ny  = fmap fromIntegral size

data GridPoint a =
  GP {
    gpXY :: {-# UNPACK #-} !(XY Double)
  , gpZ  :: {-# UNPACK #-} !a
  } deriving (Eq, Show, Read)

instance Storable a => Storable (GridPoint a) where
  sizeOf _ = sizeOf (undefined::XY Double) + sizeOf (undefined::Double)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::Double)
  {-# INLINE alignment #-}
  poke ptr (GP xy z) = poke ptr' xy >> poke (castPtr (ptr' `plusPtr` 1)) z
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = GP <$> peek ptr'
                <*> peek (castPtr (ptr' `plusPtr` 1))
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

{# enum GDALGridAlgorithm as GridAlgorithm {} with prefix = "GGA_"
     deriving (Eq,Read,Show,Bounded) #}

class Storable a => GridAlgorithmOptions a where
  gridAlgorithm :: a -> GridAlgorithm
  setNodata     :: Ptr a -> CDouble -> IO ()

{#pointer
   *GridInverseDistanceToAPowerOptions->InverseDistanceToAPowerOptions #}

data InverseDistanceToAPowerOptions =
  InverseDistanceToAPowerOptions {
    idpPower           :: !Double
  , idpSmoothing       :: !Double
  , idpAnisotropyRatio :: !Double
  , idpAnisotropyAngle :: !Double
  , idpRadius1         :: !Double
  , idpRadius2         :: !Double
  , idpAngle           :: !Double
  , idpMinPoints       :: !Int
  , idpMaxPoints       :: !Int
  } deriving (Eq, Show)

instance GridAlgorithmOptions InverseDistanceToAPowerOptions where
  gridAlgorithm = const InverseDistanceToAPower
  setNodata     = {#set GridInverseDistanceToAPowerOptions->dfNoDataValue#}

instance Default InverseDistanceToAPowerOptions where
  def = InverseDistanceToAPowerOptions {
          idpPower           = 1
        , idpSmoothing       = 1
        , idpAnisotropyRatio = 0
        , idpAnisotropyAngle = 0
        , idpRadius1         = 0
        , idpRadius2         = 0
        , idpAngle           = 0
        , idpMinPoints       = 0
        , idpMaxPoints       = 10
        }

instance Storable InverseDistanceToAPowerOptions where
  sizeOf _    = {#sizeof GDALGridInverseDistanceToAPowerOptions#}
  alignment _ = {#alignof GDALGridInverseDistanceToAPowerOptions#}
  peek _ = undefined
  poke p InverseDistanceToAPowerOptions{..}= do
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


createGridIO
  :: forall a opts. (GDALType a, GridAlgorithmOptions opts)
  => opts
  -> Maybe ProgressFun
  -> St.Vector (GridPoint a)
  -> EnvelopeReal
  -> Size
  -> IO (U.Vector (Value a))
createGridIO options progressFun points envelope size =
  withProgressFun CreateGridStopped progressFun $ \pFun ->
  withErrorHandler $
  with options $ \opts -> do
    setNodata opts ndValue
    xs <- St.unsafeThaw (St.unsafeCast (St.map (px . gpXY) points))
    ys <- St.unsafeThaw (St.unsafeCast (St.map (py . gpXY) points))
    zs <- St.unsafeThaw (St.map (toCDouble . gpZ) points)
    out <- Stm.unsafeNew (sizeLen size)
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
        gtype
        (castPtr pOut)
        pFun
        nullPtr
    liftM (stToUValue . St.map toValue) (St.unsafeFreeze out)
  where
    toValue v
      | toCDouble v == ndValue     = NoData
      | otherwise                  = Value v
    ndValue                        = toCDouble noDataVal
    noDataVal                      = (nodata :: a)
    XY nx ny                       = fmap fromIntegral size
    Envelope (XY x0 y0) (XY x1 y1) = fmap realToFrac envelope
    gtype                          = fromEnumC (dataType (Proxy :: Proxy a))


createGrid
  :: forall a opts. (GDALType a, GridAlgorithmOptions opts)
  => opts
  -> St.Vector (GridPoint a)
  -> EnvelopeReal
  -> Size
  -> Either GDALException (U.Vector (Value a))
createGrid options points envelope =
  unsafePerformIO .
  try .
  createGridIO options Nothing points envelope
