{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module GDAL.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , GenImgProjTransformer (..)
  , reprojectImage
  , autoCreateWarpedVRT
  , createWarpedVRT
  , setTransformer
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Data.Default (Default(..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, castFunPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Storable (Storable(..))
import GDAL.OSR (SpatialReference, toWkt)

import GDAL.Internal.Types
import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.GDAL
import GDAL.Internal.Util (fromEnumC)

#include "gdal.h"
#include "gdal_alg.h"
#include "gdalwarper.h"

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} deriving (Eq,Read,Show) #}

type TransformerFunc = Ptr () -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CInt

class Transformer t where
  transformerFunc     :: t -> FunPtr TransformerFunc
  createTransformer   :: Ptr () -> t -> IO (Ptr t)
  destroyTransformer  :: Ptr t -> IO ()

  destroyTransformer  = {# call GDALDestroyTransformer as ^#} . castPtr
  
data GenImgProjTransformer = forall s a.
    GenImgProjTransformer {
      giptSrcDs    :: Maybe (RODataset s a)
    , giptDstDs    :: Maybe (RWDataset s a)
    , giptSrcSrs   :: Maybe SpatialReference
    , giptDstSrs   :: Maybe SpatialReference
    , giptUseGCP   :: Bool
    , giptMaxError :: Double
    , giptOrder    :: Int
  }

instance Default GenImgProjTransformer where
  def = GenImgProjTransformer {
          giptSrcDs    = Nothing
        , giptDstDs    = Nothing
        , giptSrcSrs   = Nothing
        , giptDstSrs   = Nothing
        , giptUseGCP   = True
        , giptMaxError = 1
        , giptOrder    = 0
        }

setTransformer :: Transformer t => t -> WarpOptions -> WarpOptions
setTransformer t opts = opts {woTransfomer = Just (SomeTransformer t)}

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: FunPtr TransformerFunc

instance Transformer GenImgProjTransformer where
  transformerFunc _ = c_GDALGenImgProjTransform
  createTransformer dsPtr GenImgProjTransformer{..}
    = fmap castPtr $ throwIfError "GDALCreateGenImgProjTransformer" $ 
        withMaybeSRAsCString giptSrcSrs $ \sSr ->
        withMaybeSRAsCString giptDstSrs $ \dSr ->
          {#call GDALCreateGenImgProjTransformer as ^#}
            (maybe dsPtr (castPtr . unDataset) giptSrcDs)
            sSr
            (castPtr (maybe nullPtr unDataset giptDstDs))
            dSr
            (fromBool giptUseGCP)
            (realToFrac giptMaxError)
            (fromIntegral giptOrder)

data SomeTransformer = forall t. Transformer t => SomeTransformer t

instance Show SomeTransformer where show _ = "SomeTransformer"

data WarpOptions = 
  WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [(Int,Int)]
    , woTransfomer      :: Maybe SomeTransformer
  } deriving (Show)

instance Default WarpOptions where
  def = WarpOptions {
            woResampleAlg     = GRA_NearestNeighbour
          , woWarpOptions     = []
          , woMemoryLimit     = 0
          , woWorkingDatatype = GDT_Unknown
          , woBands           = []
          , woTransfomer      = Just (SomeTransformer (def :: GenImgProjTransformer))
          }

intListToPtr :: [Int] -> IO (Ptr CInt)
intListToPtr [] = return nullPtr
intListToPtr l = do
  ptr <- mallocArray (length l)
  mapM_ (\(i,v) -> pokeElemOff ptr i (fromIntegral v)) (zip [0..] l)
  return ptr


reprojectImage
  :: GDALType a
  => Dataset s t a
  -> Maybe SpatialReference
  -> RWDataset s a
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe WarpOptions
  -> GDAL s ()
reprojectImage srcDs srcSr dstDs dstSr alg maxError options
  | maxError < 0 = error "reprojectImage: maxError < 0"
  | otherwise
  = liftIO $ throwIfError_ "reprojectImage" $
      withLockedDatasetPtr srcDs $ \srcDsPtr ->
      withLockedDatasetPtr dstDs $ \dstDsPtr ->
      withMaybeSRAsCString srcSr $ \sSr ->
      withMaybeSRAsCString dstSr $ \dSr ->
      withWarpOptionsPtr srcDsPtr options $ \opts ->
       c_reprojectImage srcDsPtr sSr dstDsPtr dSr
                        (fromEnumC alg) 0 (fromIntegral maxError)
                        nullPtr nullPtr opts

withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString Nothing f = f nullPtr
withMaybeSRAsCString (Just srs) f = withCString (toWkt srs) f

foreign import ccall safe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset s t a) -- ^Source dataset
  -> CString             -- ^Source proj (WKT)
  -> Ptr (RWDataset s a) -- ^Dest dataset
  -> CString             -- ^Dest proj (WKT)
  -> CInt                -- ^Resample alg
  -> CDouble             -- ^Memory limit
  -> CDouble             -- ^Max error
  -> Ptr ()              -- ^Progress func (unused)
  -> Ptr ()              -- ^Progress arg (unused)
  -> Ptr WarpOptions     -- ^warp options
  -> IO CInt

autoCreateWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Maybe SpatialReference
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Maybe WarpOptions
  -> GDAL s (RODataset s a)
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
  :: Ptr (RODataset s a) -- ^Source dataset
  -> CString             -- ^Source proj (WKT)
  -> CString             -- ^Dest proj (WKT)
  -> CInt                -- ^Resample alg
  -> CDouble             -- ^Max error
  -> Ptr WarpOptions     -- ^warp options
  -> IO (Ptr (Dataset s t a))

createWarpedVRT
  :: GDALType a
  => RODataset s a
  -> Int
  -> Int
  -> Geotransform
  -> WarpOptions
  -> GDAL s (RODataset s a)
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
  :: Ptr (RODataset s a) -- ^Source dataset
  -> CInt                -- ^nPixels
  -> CInt                -- ^nLines
  -> Ptr CDouble         -- ^geotransform
  -> Ptr WarpOptions     -- ^warp options
  -> IO (Ptr (Dataset s t a))



withWarpOptionsPtr
  :: Ptr (Dataset s t b) -> Maybe WarpOptions -> (Ptr WarpOptions -> IO a) -> IO a
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
        Just (SomeTransformer t) -> do
          {#set GDALWarpOptions.pfnTransformer #} p
            (castFunPtr (transformerFunc t))
          tArg <- fmap castPtr (createTransformer (castPtr dsPtr) t)
          {#set GDALWarpOptions.pTransformerArg #} p tArg
        Nothing -> do
          {#set GDALWarpOptions.pfnTransformer #} p nullFunPtr
          {#set GDALWarpOptions.pTransformerArg #} p nullPtr
      return p

    destroyWarpOptions = c_destroyWarpOptions

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions" c_createWarpOptions
  :: IO (Ptr WarpOptions)

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions" c_destroyWarpOptions
  :: Ptr WarpOptions -> IO ()
