{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , BandOptions (..)
  , GDALWarpException (..)
  , reprojectImage
  , createWarpedVRT
  , def
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_, forM, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Data.Maybe (isJust, fromMaybe)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
import Data.Proxy (Proxy)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (
    Ptr
  , FunPtr
  , nullPtr
  , castPtr
  )
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.CPLConv
{#import GDAL.Internal.Algorithms #}
{#import GDAL.Internal.CPLError #}
{#import GDAL.Internal.CPLString #}
{#import GDAL.Internal.CPLProgress #}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.OGRGeometry #}
{#import GDAL.Internal.GDAL #}

#include "gdalwarper.h"

data GDALWarpException
  = WarpStopped
  | CannotSetTransformer
  | NonPolygonCutline
  deriving (Typeable, Show, Eq)

instance NFData GDALWarpException where
  rnf a = a `seq` ()

instance Exception GDALWarpException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} with prefix = "GRA_"
     deriving (Eq,Read,Show,Bounded) #}

data BandOptions = forall a b. (GDALType a, GDALType b)
  => BandOptions {
       biSrc       :: !Int
     , biDst       :: !Int
     , biSrcNoData :: !(Maybe a)
     , biDstNoData :: !(Maybe b)
     }

deriving instance Show BandOptions


data WarpOptions s =
  WarpOptions {
      woResampleAlg      :: ResampleAlg
    , woWarpOptions      :: OptionList
    , woMemoryLimit      :: Double
    , woWorkingDataType  :: DataType
    , woBands            :: [BandOptions]
    , woTransfomer       :: SomeTransformer s
    , woCutline          :: Maybe Geometry
    , woCutlineBlendDist :: Double
    }

{#pointer *GDALWarpOptions as WarpOptionsH newtype #}

instance Default (WarpOptions s) where
  def = WarpOptions {
          woResampleAlg      = NearestNeighbour
        , woWarpOptions      = []
        , woMemoryLimit      = 0
        , woWorkingDataType  = GDT_Unknown
        , woBands            = []
        , woTransfomer       = def
        , woCutline          = Nothing
        , woCutlineBlendDist = 0
        }

setOptionDefaults
  :: forall s t. RODataset s
  -> Maybe (Dataset s t) -> WarpOptions s -> GDAL s (WarpOptions s)
setOptionDefaults ds moDs wo@WarpOptions{..} = do
  bands <- if null woBands
            then do
              nBands <- datasetBandCount ds
              forM [1..nBands] $ \i -> do
                b <- getBand i ds
                reifyBandDataType b $ \(_ :: Proxy a) -> do
                  srcNd <- bandNodataValue b :: GDAL s (Maybe a)
                  case moDs of
                    Just oDs -> do
                      b' <- getBand i oDs
                      reifyBandDataType b' $ \(_ :: Proxy a') -> do
                        dstNd <- bandNodataValue b' :: GDAL s (Maybe a')
                        return (BandOptions i i srcNd dstNd)
                    Nothing  -> return (BandOptions i i srcNd srcNd)
            else return woBands
  let warpOptions
        | anyBandHasDstNoData wo' = ("INIT_DEST","NO_DATA") : woWarpOptions
        | otherwise               = woWarpOptions
      wo' = wo {woBands = bands}
  return wo' {woWarpOptions = warpOptions}

anyBandHasNoData :: WarpOptions s -> Bool
anyBandHasNoData wo
  = any (\BandOptions{..} -> isJust biSrcNoData || isJust biDstNoData) (woBands wo)

anyBandHasDstNoData :: WarpOptions s -> Bool
anyBandHasDstNoData wo = any (\BandOptions{..} -> isJust biDstNoData) (woBands wo)

withWarpOptionsH
  :: RODataset s
  -> Maybe Geotransform
  -> WarpOptions s
  -> (WarpOptionsH -> IO c)
  -> IO c
withWarpOptionsH _ _ WarpOptions{woCutline=Just g} _
  | geomType g /= WkbPolygon = throwBindingException NonPolygonCutline
withWarpOptionsH ds mGt wo@WarpOptions{..} act =
  withTransformerAndArg woTransfomer mGt $ \t tArg ->
  bracket (createWarpOptions t tArg)
          {#call unsafe GDALDestroyWarpOptions as ^#}
          act
  where
    DatasetH dsPtr = unDataset ds
    createWarpOptions t tArg = do
      p <- {#call unsafe GDALCreateWarpOptions as ^#}
      {#set GDALWarpOptions->hSrcDS #} p (castPtr dsPtr)
      {#set GDALWarpOptions->eResampleAlg #} p (fromEnumC woResampleAlg)
      oListPtr <- toOptionListPtr woWarpOptions
      {#set GDALWarpOptions->papszWarpOptions #} p oListPtr
      {#set GDALWarpOptions->dfWarpMemoryLimit #} p (realToFrac woMemoryLimit)
      {#set GDALWarpOptions->eWorkingDataType #} p (fromEnumC woWorkingDataType)
      {#set GDALWarpOptions->nBandCount #} p (fromIntegral (length woBands))
      {#set GDALWarpOptions->panSrcBands #} p =<<
        listToArray (map (fromIntegral . biSrc) woBands)
      {#set GDALWarpOptions->panDstBands #} p =<<
        listToArray (map (fromIntegral . biDst) woBands)
      {#set GDALWarpOptions->pfnTransformer #} p t
      {#set GDALWarpOptions->pTransformerArg #} p tArg
      {#set GDALWarpOptions->hCutline #} p =<<
        liftM castPtr (maybeCloneGeometryAndTransferOwnership woCutline)
      {#set GDALWarpOptions->dfCutlineBlendDist#} p
        (realToFrac woCutlineBlendDist)
      when (anyBandHasNoData wo) $ do
        {#set GDALWarpOptions->padfSrcNoDataReal #} p =<<
          listToArray (map (\BandOptions{..} ->
                              toCDouble (fromMaybe nodata biSrcNoData)) woBands)
        {#set GDALWarpOptions->padfDstNoDataImag #} p =<<
          listToArray (replicate (length woBands) 0)
        {#set GDALWarpOptions->padfDstNoDataReal #} p =<<
          listToArray (map (\BandOptions{..} ->
                              toCDouble (fromMaybe nodata biDstNoData)) woBands)
        {#set GDALWarpOptions->padfSrcNoDataImag #} p =<<
          listToArray (replicate (length woBands) 0)
      return p

reprojectImage
  :: RODataset s
  -> Maybe SpatialReference
  -> RWDataset s
  -> Maybe SpatialReference
  -> Double
  -> Maybe ProgressFun
  -> WarpOptions s
  -> GDAL s ()
reprojectImage _ _ _ _ _ _ WarpOptions{woTransfomer=SomeTransformer _} =
  throwBindingException CannotSetTransformer
reprojectImage srcDs srcSrs dstDs dstSrs maxError progressFun options = do
  opts@WarpOptions{..} <- setOptionDefaults srcDs (Just dstDs) options
  liftIO $
    withProgressFun WarpStopped progressFun $ \pFun ->
    withMaybeSRAsCString srcSrs $ \srcSrs' ->
    withMaybeSRAsCString dstSrs $ \dstSrs' ->
    withWarpOptionsH srcDs Nothing opts $ \wopts ->
    checkCPLError "GDALReprojectImage" $
    {#call GDALReprojectImage as ^#}
      (unDataset srcDs)
      srcSrs'
      (unDataset dstDs)
      dstSrs'
      (fromEnumC woResampleAlg)
      (realToFrac woMemoryLimit)
      (realToFrac maxError)
      pFun
      nullPtr
      wopts

createWarpedVRT
  :: forall s. RODataset s
  -> Size
  -> Geotransform
  -> WarpOptions s
  -> GDAL s (RODataset s)
createWarpedVRT srcDs (XY nPixels nLines) geotransform wo = do
  options <- setOptionDefaults srcDs Nothing (setWarpedVRTDefaultTransformer wo)
  oDs <- newDatasetHandle $
         with geotransform $ \gt ->
         withWarpOptionsH srcDs (Just geotransform) options $
         {#call unsafe GDALCreateWarpedVRT as ^#}
           (unDataset srcDs)
           (fromIntegral nPixels)
           (fromIntegral nLines)
           (castPtr gt)
  setDstNodata oDs options
  unsafeToReadOnly oDs

setWarpedVRTDefaultTransformer
  :: forall s. WarpOptions s -> WarpOptions s
setWarpedVRTDefaultTransformer wo@WarpOptions{woTransfomer=DefaultTransformer} =
  wo { woTransfomer = SomeTransformer (def :: GenImgProjTransformer s)}
setWarpedVRTDefaultTransformer wo = wo

setDstNodata :: RWDataset s -> WarpOptions s -> GDAL s ()
setDstNodata oDs options
  = when (anyBandHasDstNoData options) $
      forM_ (woBands options) $ \BandOptions{..} ->
        case biDstNoData of
          Just nd -> do
            b <- getBand biDst oDs
            setBandNodataValue b nd
          Nothing -> return ()
