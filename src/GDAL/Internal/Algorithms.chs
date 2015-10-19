{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module GDAL.Internal.Algorithms (
    Transformer (..)
  , TransformerFunc
  , GenImgProjTransformer (..)
  , GenImgProjTransformer2 (..)
  , GenImgProjTransformer3 (..)
) where

import Data.Default (Default(..))

import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)

import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.OSR (SpatialReference, withMaybeSRAsCString)
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"

class Transformer t where
  transformerFunc     :: t s a b -> TransformerFunc
  createTransformer   :: Ptr (RODataset s a) -> t s a b -> IO (Ptr (t s a b))
  destroyTransformer  :: Ptr (t s a b) -> IO ()

  destroyTransformer  = {# call GDALDestroyTransformer as ^#} . castPtr

{# pointer GDALTransformerFunc as TransformerFunc #}

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: TransformerFunc


-- ############################################################################
-- GenImgProjTransformer
-- ############################################################################

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

-- ############################################################################
-- GenImgProjTransformer2
-- ############################################################################

data GenImgProjTransformer2 s a b =
     GenImgProjTransformer2 {
      gipt2SrcDs    :: Maybe (RODataset s a)
    , gipt2DstDs    :: Maybe (RWDataset s b)
    , gipt2Options  :: OptionList
  }

instance Default (GenImgProjTransformer2 s a b) where
  def = GenImgProjTransformer2 {
          gipt2SrcDs   = Nothing
        , gipt2DstDs   = Nothing
        , gipt2Options = []
        }

instance Transformer GenImgProjTransformer2 where
  transformerFunc _ = c_GDALGenImgProjTransform
  createTransformer dsPtr GenImgProjTransformer2{..}
    = throwIfError "GDALCreateGenImgProjTransformer2" $
      withOptionList gipt2Options $ \opts ->
        c_createGenImgProjTransformer2
          (maybe dsPtr unDataset gipt2SrcDs)
          (maybe nullPtr unDataset gipt2DstDs)
          opts

foreign import ccall unsafe "gdal_alg.h GDALCreateGenImgProjTransformer2"
  c_createGenImgProjTransformer2
    :: Ptr (RODataset s a) -> Ptr (RWDataset s b) -> Ptr CString
    -> IO (Ptr (GenImgProjTransformer2 s a b))

-- ############################################################################
-- GenImgProjTransformer3
-- ############################################################################

data GenImgProjTransformer3 s a b =
     GenImgProjTransformer3 {
      gipt3SrcSrs :: Maybe SpatialReference
    , gipt3DstSrs :: Maybe SpatialReference
    , gipt3SrcGt  :: Maybe Geotransform
    , gipt3DstGt  :: Maybe Geotransform
  }

instance Default (GenImgProjTransformer3 s a b) where
  def = GenImgProjTransformer3 {
          gipt3SrcSrs = Nothing
        , gipt3DstSrs = Nothing
        , gipt3SrcGt  = Nothing
        , gipt3DstGt  = Nothing
        }

instance Transformer GenImgProjTransformer3 where
  transformerFunc _ = c_GDALGenImgProjTransform
  createTransformer dsPtr GenImgProjTransformer3{..}
    = throwIfError "GDALCreateGenImgProjTransformer3" $
      withMaybeSRAsCString gipt3SrcSrs $ \sSr ->
      withMaybeSRAsCString gipt3DstSrs $ \dSr ->
      withMaybeGeotransformPtr gipt3SrcGt $ \sGt ->
      withMaybeGeotransformPtr gipt3DstGt $ \dGt ->
        c_createGenImgProjTransformer3 sSr sGt dSr dGt

withMaybeGeotransformPtr
  :: Maybe Geotransform -> (Ptr Geotransform -> IO a) -> IO a
withMaybeGeotransformPtr Nothing   f = f nullPtr
withMaybeGeotransformPtr (Just g) f = alloca $ \gp -> poke gp g >> f gp

foreign import ccall unsafe "gdal_alg.h GDALCreateGenImgProjTransformer3"
  c_createGenImgProjTransformer3
    :: CString -> Ptr Geotransform -> CString -> Ptr Geotransform
    -> IO (Ptr (GenImgProjTransformer3 s a b))
