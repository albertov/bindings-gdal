{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , GDALWarpException (..)
  , reprojectImage
  , setTransformer
  , autoCreateWarpedVRT
  , createWarpedVRT
  , def
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, castFunPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca, malloc, free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)
{#import GDAL.Internal.Algorithms #}
{#import GDAL.Internal.CPLError #}
{#import GDAL.Internal.CPLString #}
{#import GDAL.Internal.CPLProgress #}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.GDAL #}

#include "gdal.h"
#include "gdalwarper.h"

data GDALWarpException
  = WarpStopped
  deriving (Typeable, Show, Eq)

instance NFData GDALWarpException where
  rnf a = a `seq` ()

instance Exception GDALWarpException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter}
     deriving (Eq,Read,Show) #}


data WarpOptions s a b
  = forall t. (Transformer t, Show (t s a b), GDALType a, GDALType b)
  => WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [(Int,Int)]
    , woTransfomer      :: Maybe (t s a b)
    , woSrcNodata       :: Maybe a
    , woDstNodata       :: Maybe b
    , woProgressFun     :: Maybe ProgressFun
    }

deriving instance Show (WarpOptions s a b)

instance (GDALType a, GDALType b) => Default (WarpOptions s a b) where
  def = WarpOptions {
          woResampleAlg     = GRA_NearestNeighbour
        , woWarpOptions     = []
        , woMemoryLimit     = 0
        , woWorkingDatatype = GDT_Unknown
        , woBands           = []
        , woTransfomer      = Just (def :: GenImgProjTransformer s a b)
        , woSrcNodata       = Nothing
        , woDstNodata       = Nothing
        , woProgressFun     = Nothing
        }

-- Avoids "Record update for insufficiently polymorphic field" when doigs
-- opts { woTransfomer = Just ...}
setTransformer
  :: forall t s a b. (Transformer t, Show (t s a b), GDALType a, GDALType b)
  => WarpOptions s a b -> t s a b -> WarpOptions s a b
setTransformer opts t = WarpOptions {
    woResampleAlg     = woResampleAlg opts
  , woWarpOptions     = woWarpOptions opts
  , woMemoryLimit     = woMemoryLimit opts
  , woWorkingDatatype = woWorkingDatatype opts
  , woBands           = woBands opts
  , woTransfomer      = Just t
  , woSrcNodata       = woSrcNodata opts
  , woDstNodata       = woDstNodata opts
  , woProgressFun     = woProgressFun opts
  }

withWarpOptionsPtr
  :: (GDALType a, GDALType b)
  => Ptr (RODataset s a) -> Maybe (WarpOptions s a b)
  -> (Ptr (WarpOptions s a b) -> IO c) -> IO c
withWarpOptionsPtr _ Nothing f = f nullPtr
withWarpOptionsPtr dsPtr (Just wo) f = do
  ret <- withProgressFun (woProgressFun wo) $ \pFun -> do
           (p, finalizer) <- mkWarpOptionsPtr dsPtr wo
           {#set GDALWarpOptions.pfnProgress #} p pFun
           bracket (return p) (const finalizer) f
  maybe (throwBindingException WarpStopped) return ret

mkWarpOptionsPtr
  :: (GDALType a, GDALType b)
  => Ptr (RODataset s a) -> WarpOptions s a b
  -> IO (Ptr (WarpOptions s a b), IO ())
mkWarpOptionsPtr dsPtr WarpOptions{..} = do
  p <- createWarpOptions
  return (p, destroyWarpOptions p)
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
      case woDstNodata of
        Just v -> do
          vPtr <- malloc
          poke vPtr (toNodata v)
          {#set GDALWarpOptions.padfDstNoDataReal #} p vPtr
          {#set GDALWarpOptions.padfDstNoDataImag #} p vPtr
        Nothing -> return ()
      case woSrcNodata of
        Just v -> do
          vPtr <- malloc
          poke vPtr (toNodata v)
          {#set GDALWarpOptions.padfSrcNoDataReal #} p vPtr
          {#set GDALWarpOptions.padfSrcNoDataImag #} p vPtr
        Nothing -> return ()
      return p

    destroyWarpOptions p = do
      case woTransfomer of
        Just (_ :: t s a b) -> do
          t <- {#get GDALWarpOptions.pTransformerArg #} p
          destroyTransformer (castPtr t :: Ptr (t s a b))
        Nothing -> return ()
      when (isJust woSrcNodata) $
        ({#get GDALWarpOptions.padfSrcNoDataReal #} p >>= free)
      when (isJust woDstNodata) $
        ({#get GDALWarpOptions.padfDstNoDataReal #} p >>= free)
      c_destroyWarpOptions p

    intListToPtr [] = return nullPtr
    intListToPtr l = do
      ptr <- mallocArray (length l)
      mapM_ (\(i,v) -> pokeElemOff ptr i (fromIntegral v)) (zip [0..] l)
      return ptr

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions"
  c_createWarpOptions :: IO (Ptr (WarpOptions s a b))

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions"
  c_destroyWarpOptions :: Ptr (WarpOptions s a b) -> IO ()


reprojectImage
  :: (GDALType a, GDALType b)
  => RODataset s a
  -> RWDataset s b
  -> Maybe (WarpOptions s a b)
  -> GDAL s ()
reprojectImage srcDs dstDs opts =
  liftIO $ void $ throwIfError "reprojectImage" $
    withLockedDatasetPtr srcDs $ \srcPtr ->
    withLockedDatasetPtr dstDs $ \dstPtr ->
    withWarpOptionsPtr srcPtr opts $
      c_reprojectImage srcPtr nullPtr dstPtr nullPtr 0 0 0 nullFunPtr nullPtr

foreign import ccall safe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset s t a)     -- ^Source dataset
  -> CString                 -- ^Source proj (WKT)
  -> Ptr (RWDataset s b)     -- ^Dest dataset
  -> CString                 -- ^Dest proj (WKT)
  -> CInt                    -- ^Resample alg
  -> CDouble                 -- ^Memory limit
  -> CDouble                 -- ^Max error
  -> ProgressFunPtr          -- ^Progress func
  -> Ptr ()                  -- ^Progress arg (unused)
  -> Ptr (WarpOptions s a b) -- ^warp options
  -> IO CInt

autoCreateWarpedVRT
  :: (GDALType a, GDALType b)
  => RODataset s a
  -> Maybe SpatialReference
  -> WarpOptions s a b
  -> GDAL s (RODataset s b)
autoCreateWarpedVRT ds dstSrs options = do
  (opts, finalizeOpts) <- liftIO (mkWarpOptionsPtr dsPtr options)
  registerFinalizer finalizeOpts
  newDsPtr <- liftIO $ withMaybeSRAsCString dstSrs $ \srs ->
    c_autoCreateWarpedVRT dsPtr nullPtr srs 0 0 opts
  newDerivedDatasetHandle ds newDsPtr
  where dsPtr = unDataset ds



foreign import ccall safe "gdalwarper.h GDALAutoCreateWarpedVRT" c_autoCreateWarpedVRT
  :: Ptr (RODataset s a)     -- ^Source dataset
  -> CString                 -- ^Source proj (WKT)
  -> CString                 -- ^Dest proj (WKT)
  -> CInt                    -- ^Resample alg
  -> CDouble                 -- ^Max error
  -> Ptr (WarpOptions s a b) -- ^warp options
  -> IO (Ptr (RODataset s b))

createWarpedVRT
  :: (GDALType a, GDALType b)
  => RODataset s a
  -> Int
  -> Int
  -> Geotransform
  -> WarpOptions s a b
  -> GDAL s (RODataset s b)
createWarpedVRT srcDs nPixels nLines geotransform options = do
  let dsPtr      = unDataset srcDs
      nBands     = datasetBandCount srcDs
      options'
        | null (woBands options)
        = options { woBands = map (\i -> (i,i)) [1..nBands]}
        | otherwise
        = options
  (opts,finalizeOpts) <- liftIO (mkWarpOptionsPtr dsPtr options')
  registerFinalizer finalizeOpts
  newDsPtr <- liftIO $ alloca $ \gt -> do
    poke (castPtr gt) geotransform
    {#set GDALWarpOptions.hSrcDS #} opts (castPtr dsPtr)
    pArg <- {#get GDALWarpOptions.pTransformerArg #} opts
    when (pArg /= nullPtr) $
      {#call GDALSetGenImgProjTransformerDstGeoTransform as ^#} pArg gt
    c_createWarpedVRT dsPtr (fromIntegral nPixels) (fromIntegral nLines) gt opts
  newDerivedDatasetHandle srcDs newDsPtr

foreign import ccall safe "gdalwarper.h GDALCreateWarpedVRT" c_createWarpedVRT
  :: Ptr (RODataset s a)     -- ^Source dataset
  -> CInt                    -- ^nPixels
  -> CInt                    -- ^nLines
  -> Ptr CDouble             -- ^geotransform
  -> Ptr (WarpOptions s a b) -- ^warp options
  -> IO (Ptr (RODataset s b))
