{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module GDAL.Internal.Algorithms (
    Transformer (..)
  , TransformerFunc
  , GenImgProjTransformer (..)
) where

import Data.Default (Default(..))

import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)

import GDAL.Internal.CPLError
import GDAL.Internal.OSR (SpatialReference, withMaybeSRAsCString)
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"

type TransformerFunc
  = Ptr () -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
 -> Ptr CInt -> CInt

class Transformer t where
  transformerFunc     :: t s a b -> FunPtr TransformerFunc
  createTransformer   :: Ptr (RODataset s a) -> t s a b -> IO (Ptr (t s a b))
  destroyTransformer  :: Ptr (t s a b) -> IO ()

  destroyTransformer  = {# call GDALDestroyTransformer as ^#} . castPtr

data GenImgProjTransformer s a b =
     GenImgProjTransformer {
      giptSrcDs    :: Maybe (RODataset s a)
    , giptDstDs    :: Maybe (RWDataset s b)
    , giptSrcSrs   :: Maybe SpatialReference
    , giptDstSrs   :: Maybe SpatialReference
    , giptUseGCP   :: Bool
    , giptMaxError :: Double
    , giptOrder    :: Int
  }

instance Show (GenImgProjTransformer s a b) where
  show GenImgProjTransformer{..} = concat [
    "GenImgProjTransformer { giptSrcSrs = ", show giptSrcSrs
                        , ", giptDstSrs = ", show giptDstSrs
                        , ", giptUseGCP = ", show giptUseGCP
                        , ", giptMaxError = ", show giptMaxError
                        , ", giptOrder = ", show giptOrder
                        , " }"]

instance Default (GenImgProjTransformer s a b) where
  def = GenImgProjTransformer {
          giptSrcDs    = Nothing
        , giptDstDs    = Nothing
        , giptSrcSrs   = Nothing
        , giptDstSrs   = Nothing
        , giptUseGCP   = True
        , giptMaxError = 1
        , giptOrder    = 0
        }

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: FunPtr TransformerFunc

instance Transformer GenImgProjTransformer where
  transformerFunc _ = c_GDALGenImgProjTransform
  createTransformer dsPtr GenImgProjTransformer{..}
    = throwIfError "GDALCreateGenImgProjTransformer" $
      withMaybeSRAsCString giptSrcSrs $ \sSr ->
      withMaybeSRAsCString giptDstSrs $ \dSr ->
        c_createGenImgProjTransformer
          (maybe dsPtr unDataset giptSrcDs)
          sSr
          (maybe nullPtr unDataset giptDstDs)
          dSr
          (fromBool giptUseGCP)
          (realToFrac giptMaxError)
          (fromIntegral giptOrder)

foreign import ccall unsafe "gdal_alg.h GDALCreateGenImgProjTransformer"
  c_createGenImgProjTransformer
    :: Ptr (RODataset s a) -> CString -> Ptr (Dataset s m b) -> CString -> CInt
    -> CDouble -> CInt -> IO (Ptr (GenImgProjTransformer s a b))
