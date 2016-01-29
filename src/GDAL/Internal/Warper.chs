{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.Warper (
    ResampleAlg (..)
  , WarpOptions
  , BandOptions (..)
  , GDALWarpException (..)
  , HasResampleAlg (..)
  , HasMemoryLimit (..)
  , HasWorkingDataType (..)
  , HasCutline (..)
  , HasCutlineBlendDist (..)
  , HasSrcSrs (..)
  , HasDstSrs (..)
  , HasMaxError (..)
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
import Lens.Micro

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

class HasResampleAlg o a | o -> a where
  resampleAlg :: Lens' o a

class HasMemoryLimit o a | o -> a where
  memoryLimit :: Lens' o a

class HasWorkingDataType o a | o -> a where
  workingDataType :: Lens' o a

class HasCutline o a | o -> a where
  cutline :: Lens' o a

class HasCutlineBlendDist o a | o -> a where
  cutlineBlendList :: Lens' o a


data WarpOptions a =
  WarpOptions {
      woResampleAlg      :: ResampleAlg
    , woWarpOptions      :: OptionList
    , woMemoryLimit      :: Double
    , woWorkingDataType  :: DataTypeK
    , woBands            :: [BandOptions a]
    , woTransfomer       :: Maybe SomeTransformer
    , woCutline          :: Maybe Geometry
    , woCutlineBlendDist :: Double
    , woSrcSrs           :: Maybe SpatialReference
    , woDstSrs           :: Maybe SpatialReference
    , woMaxError         :: Double
    , woProgressFun      :: Maybe ProgressFun
    }

instance HasResampleAlg (WarpOptions a) ResampleAlg where
  resampleAlg = lens woResampleAlg (\o a -> o {woResampleAlg = a})

instance HasOptions (WarpOptions a) OptionList where
  options = lens woWarpOptions (\o a -> o {woWarpOptions = a})

instance HasMemoryLimit (WarpOptions a) Double where
  memoryLimit = lens woMemoryLimit (\o a -> o {woMemoryLimit = a})

instance HasWorkingDataType (WarpOptions a) DataTypeK where
  workingDataType = lens woWorkingDataType (\o a -> o {woWorkingDataType = a})

instance HasBands (WarpOptions a) [BandOptions a] where
  bands = lens woBands (\o a -> o {woBands = a})

instance HasTransformer (WarpOptions a) (Maybe SomeTransformer) where
  transformer = lens woTransfomer (\o a -> o {woTransfomer = a})

instance HasCutline (WarpOptions a) (Maybe Geometry) where
  cutline = lens woCutline (\o a -> o {woCutline = a})

instance HasCutlineBlendDist (WarpOptions a) Double where
  cutlineBlendList =
    lens woCutlineBlendDist (\o a -> o {woCutlineBlendDist = a})

instance HasSrcSrs (WarpOptions a) (Maybe SpatialReference) where
  srcSrs = lens woSrcSrs (\o a -> o {woSrcSrs = a})

instance HasDstSrs (WarpOptions a) (Maybe SpatialReference) where
  dstSrs = lens woDstSrs (\o a -> o {woDstSrs = a})

instance HasMaxError (WarpOptions a) Double where
  maxError = lens woMaxError (\o a -> o {woMaxError = a})

instance HasProgressFun (WarpOptions a) (Maybe ProgressFun) where
  progressFun = lens woProgressFun (\o a -> o {woProgressFun = a})

{#pointer *GDALWarpOptions as WarpOptionsH newtype #}

instance Default (WarpOptions a) where
  def = WarpOptions {
          woResampleAlg      = NearestNeighbour
        , woWarpOptions      = []
        , woMemoryLimit      = 0
        , woWorkingDataType  = GUnknown
        , woBands            = []
        , woTransfomer       = def
        , woCutline          = Nothing
        , woCutlineBlendDist = 0
        , woSrcSrs           = Nothing
        , woDstSrs           = Nothing
        , woMaxError         = 0
        , woProgressFun      = Nothing
        }

setOptionDefaults
  :: GDALType a
  => RODataset s a -> Maybe (Dataset s a t) -> WarpOptions a
  -> GDAL s (WarpOptions a)
setOptionDefaults ds moDs wo@WarpOptions{..} = do
  bs <- if null woBands
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
      wo' = wo {woBands = bs}
  return wo' {woWarpOptions = warpOptions}

anyBandHasNoData :: WarpOptions a -> Bool
anyBandHasNoData wo
  = any (\BandOptions{..} -> isJust biSrcNoData || isJust biDstNoData) (woBands wo)

anyBandHasDstNoData :: WarpOptions a -> Bool
anyBandHasDstNoData wo = any (\BandOptions{..} -> isJust biDstNoData) (woBands wo)

withWarpOptionsH
  :: GDALType a
  => RODataset s a
  -> Maybe Geotransform
  -> WarpOptions a
  -> (WarpOptionsH -> IO c)
  -> IO c
withWarpOptionsH _ _ WarpOptions{woCutline=Just g} _
  | geomType g /= WkbPolygon = throwBindingException NonPolygonCutline
withWarpOptionsH ds mGt wo@WarpOptions{..} act =
  withTransformerAndArg woTransfomer (Just ds) mGt $ \t tArg ->
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
  -> RWDataset s a
  -> WarpOptions a
  -> GDAL s ()
reprojectImage _ _ WarpOptions{woTransfomer=Just _} =
  throwBindingException CannotSetTransformer
reprojectImage srcDs dstDs cfg = do
  cfg' <- setOptionDefaults srcDs (Just dstDs) cfg
  liftIO $
    withProgressFun "reprojectImage" (cfg^.progressFun) $ \pFun ->
    withMaybeSRAsCString (cfg'^.srcSrs) $ \srcSrs' ->
    withMaybeSRAsCString (cfg'^.dstSrs) $ \dstSrs' ->
    withWarpOptionsH srcDs Nothing cfg' $ \wopts ->
    checkCPLError "GDALReprojectImage" $
    {#call GDALReprojectImage as ^#}
      (unDataset srcDs)
      srcSrs'
      (unDataset dstDs)
      dstSrs'
      (fromEnumC (cfg'^.resampleAlg))
      (realToFrac (cfg'^.memoryLimit))
      (realToFrac (cfg'^.maxError))
      pFun
      nullPtr
      wopts

createWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Size
  -> Geotransform
  -> WarpOptions a
  -> GDAL s (RODataset s a)
createWarpedVRT srcDs (nPixels :+: nLines) geotransform wo = do
  wo' <- setOptionDefaults srcDs Nothing $ wo
    & transformer %~ maybe (Just (SomeTransformer gipt)) Just
  oDs <- newDatasetHandle $
         with geotransform $ \gt ->
         withWarpOptionsH srcDs (Just geotransform) wo' $
         {#call unsafe GDALCreateWarpedVRT as ^#}
           (unDataset srcDs)
           (fromIntegral nPixels)
           (fromIntegral nLines)
           (castPtr gt)
  setDstNodata oDs wo'
  unsafeToReadOnly oDs

setDstNodata :: GDALType a => RWDataset s a -> WarpOptions a -> GDAL s ()
setDstNodata oDs cfg
  = when (anyBandHasDstNoData cfg) $
      forM_ (woBands cfg) $ \BandOptions{..} ->
        case biDstNoData of
          Just nd -> do
            getBand biDst oDs >>= setBandNodataValue nd
          Nothing -> return ()
