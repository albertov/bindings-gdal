{-# LANGUAGE ForeignFunctionInterface #-}
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
  , setTransformer
  , autoCreateWarpedVRT
  , createWarpedVRT
  , def
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Data.Maybe (isJust, fromMaybe)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (
    Ptr
  , FunPtr
  , nullPtr
  , castPtr
  , castFunPtr
  , nullFunPtr
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.Algorithms
import GDAL.Internal.CPLConv
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

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} with prefix = "GRA_"
     deriving (Eq,Read,Show,Bounded) #}

data BandOptions a b
  = BandOptions {
      biSrc       :: !Int
    , biDst       :: !Int
    , biSrcNoData :: !(Maybe a)
    , biDstNoData :: !(Maybe b)
    } deriving Show


data WarpOptions s a b
  = forall t. (Transformer t, Show (t s a b), GDALType a, GDALType b)
  => WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [BandOptions a b]
    , woTransfomer      :: Maybe (t s a b)
    , woProgressFun     :: Maybe ProgressFun
    }

deriving instance Show (WarpOptions s a b)

instance (GDALType a, GDALType b) => Default (WarpOptions s a b) where
  def = WarpOptions {
          woResampleAlg     = NearestNeighbour
        , woWarpOptions     = []
        , woMemoryLimit     = 0
        , woWorkingDatatype = GDT_Unknown
        , woBands           = []
        , woTransfomer      = Just (def :: GenImgProjTransformer s a b)
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
  , woProgressFun     = woProgressFun opts
  }



setOptionDefaults
  :: (GDALType a, GDALType b)
  => RODataset s a -> Maybe (Dataset s t b) -> WarpOptions s a b
  -> GDAL s (WarpOptions s a b)
setOptionDefaults ds moDs wo@WarpOptions{..} = do
  bands <- if null woBands
            then forM [1..datasetBandCount ds] $ \i -> do
              srcNd <- getBand i ds >>= bandNodataValue
              dstNd <- case moDs of
                         Just oDs -> getBand i oDs >>= bandNodataValue
                         Nothing  -> return (fmap (fromNodata . toNodata) srcNd)
              return (BandOptions i i srcNd dstNd)
            else return woBands
  let warpOptions
        | any (isJust . biDstNoData) bands
        = ("INIT_DEST","NO_DATA") : woWarpOptions
        | otherwise = woWarpOptions
  return (wo { woWarpOptions = warpOptions, woBands = bands})

anyBandHasNoData :: WarpOptions s a b -> Bool
anyBandHasNoData wo
  = any (\BandOptions{..} -> isJust biSrcNoData || isJust biDstNoData) (woBands wo)

anyBandHasDstNoData :: WarpOptions s a b -> Bool
anyBandHasDstNoData wo = any (\BandOptions{..} -> isJust biDstNoData) (woBands wo)

withWarpOptionsPtr
  :: (GDALType a, GDALType b)
  => RODataset s a -> WarpOptions s a b -> (Ptr (WarpOptions s a b) -> IO c)
  -> IO c
withWarpOptionsPtr ds wo f = do
  (opts, finalize) <- mkWarpOptionsPtr ds wo
  bracket (return opts) (const finalize) f

mkWarpOptionsPtr
  :: (GDALType a, GDALType b)
  => RODataset s a -> WarpOptions s a b
  -> IO (Ptr (WarpOptions s a b), IO ())
mkWarpOptionsPtr ds wo@WarpOptions{..} = do
  p <- createWarpOptions
  return (p, destroyWarpOptions p)
  where
    dsPtr = unDataset ds
    createWarpOptions = do
      p <- c_createWarpOptions
      {#set GDALWarpOptions.hSrcDS #} p (castPtr dsPtr)
      {#set GDALWarpOptions.eResampleAlg #} p (fromEnumC woResampleAlg)
      oListPtr <- toOptionListPtr woWarpOptions
      {#set GDALWarpOptions.papszWarpOptions #} p oListPtr
      {#set GDALWarpOptions.dfWarpMemoryLimit #} p (realToFrac woMemoryLimit)
      {#set GDALWarpOptions.eWorkingDataType #} p (fromEnumC woWorkingDatatype)
      {#set GDALWarpOptions.nBandCount #} p (fromIntegral (length woBands))
      {#set GDALWarpOptions.panSrcBands #} p =<<
        listToPtr (map (fromIntegral . biSrc) woBands)
      {#set GDALWarpOptions.panDstBands #} p =<<
        listToPtr (map (fromIntegral . biDst) woBands)
      case woTransfomer of
        Just t -> do
          {#set GDALWarpOptions.pfnTransformer #} p
            (castFunPtr (transformerFunc t))
          tArg <- fmap castPtr (createTransformer dsPtr t)
          {#set GDALWarpOptions.pTransformerArg #} p tArg
        Nothing -> do
          {#set GDALWarpOptions.pfnTransformer #} p nullFunPtr
          {#set GDALWarpOptions.pTransformerArg #} p nullPtr
      when (anyBandHasNoData wo) $ do
        vPtrSrcR <- listToPtr
                      (map (toNodata . fromMaybe nodata . biSrcNoData) woBands)
        vPtrSrcI <- listToPtr (replicate (length woBands) 0)
        vPtrDstR <- listToPtr
                      (map (toNodata . fromMaybe nodata . biDstNoData) woBands)
        vPtrDstI <- listToPtr (replicate (length woBands) 0)
        {#set GDALWarpOptions.padfDstNoDataReal #} p vPtrDstR
        {#set GDALWarpOptions.padfDstNoDataImag #} p vPtrDstI
        {#set GDALWarpOptions.padfSrcNoDataReal #} p vPtrSrcR
        {#set GDALWarpOptions.padfSrcNoDataImag #} p vPtrSrcI
      return p

    destroyWarpOptions p = do
      case woTransfomer of
        Just (_ :: t s a b) -> do
          t <- {#get GDALWarpOptions.pTransformerArg #} p
          destroyTransformer (castPtr t :: Ptr (t s a b))
        Nothing -> return ()
      c_destroyWarpOptions p

    listToPtr [] = return nullPtr
    listToPtr l = do
      ptr <- cplMallocArray (length l)
      mapM_ (\(i,v) -> pokeElemOff ptr i v) (zip [0..] l)
      return ptr

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions"
  c_createWarpOptions :: IO (Ptr (WarpOptions s a b))

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions"
  c_destroyWarpOptions :: Ptr (WarpOptions s a b) -> IO ()

reprojectImage
  :: (GDALType a, GDALType b)
  => RODataset s a
  -> Maybe SpatialReference
  -> RWDataset s b
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Double
  -> Double
  -> Maybe ProgressFun
  -> OptionList
  -> GDAL s ()
reprojectImage srcDs srcSrs dstDs dstSrs algo memLimit maxError progressFun opts
  = do options' <- setOptionDefaults srcDs (Just dstDs)
                     (def {woWarpOptions=opts})
       ret <- liftIO $
          withProgressFun progressFun $ \pFun ->
          throwIfError "reprojectImage" $
          withLockedDatasetPtr srcDs $ \srcPtr ->
          withLockedDatasetPtr dstDs $ \dstPtr ->
          withMaybeSRAsCString srcSrs $ \srcSrs' ->
          withMaybeSRAsCString dstSrs $ \dstSrs' ->
          withWarpOptionsPtr srcDs options' $ \wopts ->
            void $ c_reprojectImage srcPtr srcSrs' dstPtr dstSrs' algo'
                     memLimit' maxError' pFun nullPtr wopts
       maybe (throwBindingException WarpStopped) return ret
  where
    algo'     = fromEnumC algo
    maxError' = realToFrac maxError
    memLimit' = realToFrac memLimit

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
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Double
  -> OptionList
  -> GDAL s (RODataset s b)
autoCreateWarpedVRT ds srcSrs dstSrs algo maxError opts = do
  options' <- setOptionDefaults ds Nothing (def {woWarpOptions=opts})
  oDsPtr <- liftIO $
    withMaybeSRAsCString srcSrs $ \srcSrs' ->
    withMaybeSRAsCString dstSrs $ \dstSrs' ->
    withWarpOptionsPtr ds options' $ \wopts ->
    c_autoCreateWarpedVRT (unDataset ds) srcSrs' dstSrs' algo' maxError' wopts
  oDs <- newDerivedDatasetHandle ds oDsPtr
  setDstNodata oDs options'
  unsafeToReadOnly oDs
  where
    algo'     = fromEnumC algo
    maxError' = realToFrac maxError



foreign import ccall safe "gdalwarper.h GDALAutoCreateWarpedVRT" c_autoCreateWarpedVRT
  :: Ptr (RODataset s a)     -- ^Source dataset
  -> CString                 -- ^Source proj (WKT)
  -> CString                 -- ^Dest proj (WKT)
  -> CInt                    -- ^Resample alg
  -> CDouble                 -- ^Max error
  -> Ptr (WarpOptions s a b) -- ^warp options
  -> IO (Ptr (RWDataset s b))

createWarpedVRT
  :: (GDALType a, GDALType b)
  => RODataset s a
  -> Size
  -> Geotransform
  -> WarpOptions s a b
  -> GDAL s (RODataset s b)
createWarpedVRT srcDs (XY nPixels nLines) geotransform options = do
  let dsPtr = unDataset srcDs
  options' <- setOptionDefaults srcDs Nothing options
  (opts,finalizeOpts) <- liftIO (mkWarpOptionsPtr srcDs options')
  registerFinalizer finalizeOpts
  newDsPtr <- liftIO $ alloca $ \gt -> do
    poke (castPtr gt) geotransform
    pArg <- {#get GDALWarpOptions.pTransformerArg #} opts
    when (pArg /= nullPtr) $
      {#call GDALSetGenImgProjTransformerDstGeoTransform as ^#} pArg gt
    c_createWarpedVRT dsPtr (fromIntegral nPixels) (fromIntegral nLines) gt opts
  oDs <- newDerivedDatasetHandle srcDs newDsPtr
  setDstNodata oDs options'
  unsafeToReadOnly oDs

setDstNodata :: GDALType b => RWDataset s b -> WarpOptions s a b -> GDAL s ()
setDstNodata oDs options
  = when (anyBandHasDstNoData options) $
      forM_ (woBands options) $ \BandOptions{..} ->
        case biDstNoData of
          Just nd -> do
            b <- getBand biDst oDs
            setBandNodataValue b nd
          Nothing -> return ()

foreign import ccall safe "gdalwarper.h GDALCreateWarpedVRT" c_createWarpedVRT
  :: Ptr (RODataset s a)     -- ^Source dataset
  -> CInt                    -- ^nPixels
  -> CInt                    -- ^nLines
  -> Ptr CDouble             -- ^geotransform
  -> Ptr (WarpOptions s a b) -- ^warp options
  -> IO (Ptr (RWDataset s b))
