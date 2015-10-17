{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module GDAL.Internal.Algorithms (
    Transformer (..)
  , SomeTransformer (..)
  , TransformerFunc
  , GenImgProjTransformer (..)
) where

import Data.Default (Default(..))

import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)

import GDAL.Internal.CPLError
import GDAL.Internal.OSR (SpatialReference, withMaybeSRAsCString)
import GDAL.Internal.GDAL

#include "gdal_alg.h"

type TransformerFunc
  = Ptr () -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
 -> Ptr CInt -> CInt

class Transformer t where
  transformerFunc     :: t -> FunPtr TransformerFunc
  createTransformer   :: Ptr () -> t -> IO (Ptr t)
  destroyTransformer  :: Ptr t -> IO ()

  destroyTransformer  = {# call GDALDestroyTransformer as ^#} . castPtr

data SomeTransformer = forall t. Transformer t => SomeTransformer t

instance Show SomeTransformer where
  show _ = "SomeTransformer"

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
