{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , reprojectImage
  , withTransformer
  , autoCreateWarpedVRT
  , createWarpedVRT
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, castFunPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Algorithms
import GDAL.Internal.Types
import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.OSR (SpatialReference, withMaybeSRAsCString)
import GDAL.Internal.GDAL
import GDAL.Internal.Util (fromEnumC)

#include "gdal.h"
#include "gdalwarper.h"

data GDALWarpException
  = InvalidMaxError !Int
  | WarpStopped
  deriving (Typeable, Show, Eq)

instance NFData GDALWarpException where
  rnf a = a `seq` ()

instance Exception GDALWarpException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter}
     deriving (Eq,Read,Show) #}


data WarpOptions s a = forall t. (Transformer t, Show (t s a))
  => WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [(Int,Int)]
    , woTransfomer      :: Maybe (t s a)
    }

deriving instance Show (WarpOptions s a)

instance Default (WarpOptions s a) where
  def = WarpOptions {
          woResampleAlg     = GRA_NearestNeighbour
        , woWarpOptions     = []
        , woMemoryLimit     = 0
        , woWorkingDatatype = GDT_Unknown
        , woBands           = []
        , woTransfomer      = Just (def :: GenImgProjTransformer s a)
        }

-- Avoids "Record update for insufficiently polymorphic field" when doigs
-- opts { woTransfomer = Just ...}
withTransformer
  :: forall t s a. (Transformer t, Show (t s a))
  => WarpOptions s a -> t s a -> WarpOptions s a
withTransformer opts t = WarpOptions {
    woResampleAlg     = woResampleAlg opts
  , woWarpOptions     = woWarpOptions opts
  , woMemoryLimit     = woMemoryLimit opts
  , woWorkingDatatype = woWorkingDatatype opts
  , woBands           = woBands opts
  , woTransfomer      = Just t
  }

withWarpOptionsPtr
  :: Ptr (RODataset s a) -> Maybe (WarpOptions s a)
  -> (Ptr (WarpOptions s a) -> IO b) -> IO b
withWarpOptionsPtr _ Nothing  f = f nullPtr
withWarpOptionsPtr dsPtr (Just (WarpOptions{..})) f
  = bracket createWarpOptions destroyWarpOptions f
  where
    createWarpOptions = do
      p <- c_createWarpOptions
      {#set GDALWarpOptions.eResampleAlg #} p (fromEnumC woResampleAlg)
      oListPtr <- toOptionListPtr woWarpOptions
      {#set GDALWarpOptions.papszWarpOptions #} p oListPtr
      {#set GDALWarpOptions.dfWarpMemoryLimit #} p (realToFrac woMemoryLimit)
      {#set GDALWarpOptions.eWorkingDataType #} p (fromEnumC woWorkingDatatype)
      {#set GDALWarpOptions.nBandCount #} p (fromIntegral (length woBands))
      {#set GDALWarpOptions.panSrcBands #} p =<< intListToPtr (map fst woBands)
      {#set GDALWarpOptions.panDstBands #} p =<< intListToPtr (map snd woBands)
      case woTransfomer of
        Just t -> do
          {#set GDALWarpOptions.pfnTransformer #} p
            (castFunPtr (transformerFunc t))
          tArg <- fmap castPtr (createTransformer dsPtr t)
          {#set GDALWarpOptions.pTransformerArg #} p tArg
        Nothing -> do
          {#set GDALWarpOptions.pfnTransformer #} p nullFunPtr
          {#set GDALWarpOptions.pTransformerArg #} p nullPtr
      return p

    destroyWarpOptions = c_destroyWarpOptions

    intListToPtr [] = return nullPtr
    intListToPtr l = do
      ptr <- mallocArray (length l)
      mapM_ (\(i,v) -> pokeElemOff ptr i (fromIntegral v)) (zip [0..] l)
      return ptr

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions"
  c_createWarpOptions :: IO (Ptr (WarpOptions s a))

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions"
  c_destroyWarpOptions :: Ptr (WarpOptions s a) -> IO ()


reprojectImage
  :: GDALType a
  => RODataset s a
  -> Maybe SpatialReference
  -> RWDataset s b
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe ProgressFun
  -> Maybe (WarpOptions s a)
  -> GDAL s ()
reprojectImage srcDs srcSr dstDs dstSr alg maxError mProgressFun options
  | maxError < 0 = throwBindingException (InvalidMaxError maxError)
  | otherwise
  = do ret <- liftIO $ throwIfError "reprojectImage" $
                withLockedDatasetPtr srcDs $ \srcDsPtr ->
                withLockedDatasetPtr dstDs $ \dstDsPtr ->
                withMaybeSRAsCString srcSr $ \sSr ->
                withMaybeSRAsCString dstSr $ \dSr ->
                withWarpOptionsPtr srcDsPtr options $ \opts ->
                withProgressFun mProgressFun $ \pFunc -> void $
                 c_reprojectImage srcDsPtr sSr dstDsPtr dSr
                                  (fromEnumC alg) 0 (fromIntegral maxError)
                                  pFunc nullPtr opts
       maybe (throwBindingException WarpStopped) return ret

foreign import ccall safe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset s t a)   -- ^Source dataset
  -> CString               -- ^Source proj (WKT)
  -> Ptr (RWDataset s b)   -- ^Dest dataset
  -> CString               -- ^Dest proj (WKT)
  -> CInt                  -- ^Resample alg
  -> CDouble               -- ^Memory limit
  -> CDouble               -- ^Max error
  -> ProgressFunPtr        -- ^Progress func
  -> Ptr ()                -- ^Progress arg (unused)
  -> Ptr (WarpOptions s a) -- ^warp options
  -> IO CInt

autoCreateWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Maybe SpatialReference
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe (WarpOptions s a)
  -> GDAL s (RODataset s b)
autoCreateWarpedVRT srcDs srcSr dstSr alg maxError options = do
  newDsPtr <- liftIO $
    withLockedDatasetPtr srcDs $ \srcDsPtr ->
    withMaybeSRAsCString srcSr $ \sSr ->
    withMaybeSRAsCString dstSr $ \dSr ->
    withWarpOptionsPtr srcDsPtr options $ \opts ->
      c_autoCreateWarpedVRT srcDsPtr sSr dSr (fromEnumC alg)
                            (fromIntegral maxError) opts
  newDerivedDatasetHandle srcDs newDsPtr



foreign import ccall safe "gdalwarper.h GDALAutoCreateWarpedVRT" c_autoCreateWarpedVRT
  :: Ptr (RODataset s a)   -- ^Source dataset
  -> CString               -- ^Source proj (WKT)
  -> CString               -- ^Dest proj (WKT)
  -> CInt                  -- ^Resample alg
  -> CDouble               -- ^Max error
  -> Ptr (WarpOptions s a) -- ^warp options
  -> IO (Ptr (RODataset s b))

createWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Int
  -> Int
  -> Geotransform
  -> WarpOptions s a
  -> GDAL s (RODataset s b)
createWarpedVRT srcDs nPixels nLines gt options = do
  let dsPtr      = unDataset srcDs
      nBands     = datasetBandCount srcDs
      options'
        | null (woBands options)
        = options { woBands = map (\i -> (i,i)) [1..nBands]}
        | otherwise
        = options
  newDsPtr <- liftIO $
    withWarpOptionsPtr dsPtr (Just options') $ \opts -> do
      ptr <- alloca $ \gtPtr -> do
        poke (castPtr gtPtr) gt
        {#set GDALWarpOptions.hSrcDS #} opts (castPtr dsPtr)
        pArg <- {#get GDALWarpOptions.pTransformerArg #} opts
        when (pArg /= nullPtr) $
          {#call GDALSetGenImgProjTransformerDstGeoTransform as ^#} pArg gtPtr
        c_createWarpedVRT dsPtr (fromIntegral nPixels) (fromIntegral nLines)
                          gtPtr opts
      return ptr
  newDerivedDatasetHandle srcDs newDsPtr

foreign import ccall safe "gdalwarper.h GDALCreateWarpedVRT" c_createWarpedVRT
  :: Ptr (RODataset s a)   -- ^Source dataset
  -> CInt                  -- ^nPixels
  -> CInt                  -- ^nLines
  -> Ptr CDouble           -- ^geotransform
  -> Ptr (WarpOptions s a) -- ^warp options
  -> IO (Ptr (RODataset s b))
