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
import Control.Exception (Exception(..), bracket)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
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
  = CannotSetTransformer
  | NonPolygonCutline
  deriving (Typeable, Show, Eq)

instance Exception GDALWarpException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} with prefix = "GRA_"
     deriving (Eq,Read,Show,Bounded) #}

data BandOptions a =
  BandOptions {
    biSrc       :: !Int
  , biDst       :: !Int
  , biSrcNoData :: !(Maybe a)
  , biDstNoData :: !(Maybe a)
  } deriving Show


data WarpOptions s a =
  WarpOptions {
      woResampleAlg      :: ResampleAlg
    , woWarpOptions      :: OptionList
    , woMemoryLimit      :: Double
    , woWorkingDataType  :: DataTypeK
    , woBands            :: [BandOptions a]
    , woTransfomer       :: SomeTransformer s a
    , woCutline          :: Maybe Geometry
    , woCutlineBlendDist :: Double
    }

{#pointer *GDALWarpOptions as WarpOptionsH newtype #}

instance Default (WarpOptions s a) where
  def = WarpOptions {
          woResampleAlg      = NearestNeighbour
        , woWarpOptions      = []
        , woMemoryLimit      = 0
        , woWorkingDataType  = GUnknown
        , woBands            = []
        , woTransfomer       = def
        , woCutline          = Nothing
        , woCutlineBlendDist = 0
        }

setOptionDefaults
  :: GDALType a
  => RODataset s a -> Maybe (Dataset s a t) -> WarpOptions s a
  -> GDAL s (WarpOptions s a)
setOptionDefaults ds moDs wo@WarpOptions{..} = do
  bands <- if null woBands
            then do
              nBands <- datasetBandCount ds
              forM [1..nBands] $ \i -> do
                srcNd <- bandNodataValue =<< getBand i ds
                case moDs of
                  Just oDs -> do
                    dstNd <- bandNodataValue =<< getBand i oDs
                    return (BandOptions i i srcNd dstNd)
                  Nothing  -> return (BandOptions i i srcNd srcNd)
            else return woBands
  let warpOptions
        | anyBandHasDstNoData wo' = ("INIT_DEST","NO_DATA") : woWarpOptions
        | otherwise               = woWarpOptions
      wo' = wo {woBands = bands}
  return wo' {woWarpOptions = warpOptions}

anyBandHasNoData :: WarpOptions s a -> Bool
anyBandHasNoData wo
  = any (\BandOptions{..} -> isJust biSrcNoData || isJust biDstNoData) (woBands wo)

anyBandHasDstNoData :: WarpOptions s a -> Bool
anyBandHasDstNoData wo = any (\BandOptions{..} -> isJust biDstNoData) (woBands wo)

withWarpOptionsH
  :: GDALType a
  => RODataset s a
  -> Maybe Geotransform
  -> WarpOptions s a
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
        cplNewArray (map (fromIntegral . biSrc) woBands)
      {#set GDALWarpOptions->panDstBands #} p =<<
        cplNewArray (map (fromIntegral . biDst) woBands)
      {#set GDALWarpOptions->pfnTransformer #} p t
      {#set GDALWarpOptions->pTransformerArg #} p tArg
      {#set GDALWarpOptions->hCutline #} p =<<
        liftM castPtr (maybeCloneGeometryAndTransferOwnership woCutline)
      {#set GDALWarpOptions->dfCutlineBlendDist#} p
        (realToFrac woCutlineBlendDist)
      when (anyBandHasNoData wo) $ do
        let sNds = map (\bo -> maybe (0/0) toCDouble (biSrcNoData bo)) woBands
            dNds = map (\bo -> maybe (0/0) toCDouble (biDstNoData bo)) woBands
            imgs = replicate (length woBands) 0
        cplNewArray sNds >>=
          {#set GDALWarpOptions->padfSrcNoDataReal #} p . castPtr
        cplNewArray imgs >>= {#set GDALWarpOptions->padfSrcNoDataImag #} p
        cplNewArray dNds >>=
          {#set GDALWarpOptions->padfDstNoDataReal #} p . castPtr
        cplNewArray imgs >>= {#set GDALWarpOptions->padfDstNoDataImag #} p
      return p

reprojectImage
  :: GDALType a
  => RODataset s a
  -> Maybe SpatialReference
  -> RWDataset s a
  -> Maybe SpatialReference
  -> Double
  -> Maybe ProgressFun
  -> WarpOptions s a
  -> GDAL s ()
reprojectImage _ _ _ _ _ _ WarpOptions{woTransfomer=SomeTransformer _} =
  throwBindingException CannotSetTransformer
reprojectImage srcDs srcSrs dstDs dstSrs maxError progressFun options = do
  opts@WarpOptions{..} <- setOptionDefaults srcDs (Just dstDs) options
  liftIO $
    withProgressFun "reprojectImage" progressFun $ \pFun ->
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
  :: GDALType a
  => RODataset s a
  -> Size
  -> Geotransform
  -> WarpOptions s a
  -> GDAL s (RODataset s a)
createWarpedVRT srcDs (nPixels :+: nLines) geotransform wo = do
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
  :: GDALType a => WarpOptions s a -> WarpOptions s a
setWarpedVRTDefaultTransformer w@WarpOptions{woTransfomer=DefaultTransformer} =
  w { woTransfomer = SomeTransformer (def :: GenImgProjTransformer s a)}
setWarpedVRTDefaultTransformer wo = wo

setDstNodata :: GDALType a => RWDataset s a -> WarpOptions s a -> GDAL s ()
setDstNodata oDs options
  = when (anyBandHasDstNoData options) $
      forM_ (woBands options) $ \BandOptions{..} ->
        case biDstNoData of
          Just nd -> do
            getBand biDst oDs >>= setBandNodataValue nd
          Nothing -> return ()
