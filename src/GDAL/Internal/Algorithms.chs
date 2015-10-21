{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}

module GDAL.Internal.Algorithms (
    Transformer (..)
  , TransformerFun
  , GenImgProjTransformer (..)
  , GenImgProjTransformer2 (..)
  , GenImgProjTransformer3 (..)

  , createTransformerAndArg
  , getTransformerFunPtr

  , rasterizeLayersBuf
  , rasterizeLayersBufIO
) where

{#context lib = "gdal" prefix = "GDAL" #}

import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(..))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (poke)

import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.Types
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.CPLProgress#}
{#import GDAL.Internal.OGR#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.GDAL#}

#include "gdal_alg.h"

data GDALAlgorithmException
  = RasterizeStopped
  deriving (Typeable, Show, Eq)

instance NFData GDALAlgorithmException where
  rnf a = a `seq` ()

instance Exception GDALAlgorithmException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

class Transformer t where
  createTransformerFun  :: t s -> IO (TransformerFun t s)
  destroyTransformerFun :: TransformerFun t s -> IO ()
  createTransformerArg   :: t s -> IO (Ptr (t s))
  destroyTransformerArg  :: Ptr (t s) -> IO ()

  destroyTransformerArg  = {# call GDALDestroyTransformer as ^#} . castPtr
  destroyTransformerFun = const (return ())

createTransformerAndArg
  :: Transformer t
  => Maybe (t s) -> IO (TransformerFun t s, Ptr (t s), IO ())
createTransformerAndArg Nothing = return (TransformerFun nullFunPtr, nullPtr, return ())
createTransformerAndArg (Just tr) = do
  arg <- createTransformerArg tr
  t <- createTransformerFun tr
  return (t, arg, (destroyTransformerFun t >> destroyTransformerArg arg))

newtype TransformerFun (t :: * -> *) s
  = TransformerFun {getTransformerFunPtr :: TransformerFunPtr}

{#pointer GDALTransformerFunc as TransformerFunPtr #}

withTransformerAndArg
  :: Transformer t
  => Maybe (t s) -> (TransformerFun t s -> Ptr (t s) -> IO c)
  -> IO c
withTransformerAndArg t f = do
  bracket (createTransformerAndArg t) (\(_,_,d) -> d) (\(tr,arg,_) -> f tr arg)


-- ############################################################################
-- GenImgProjTransformer
-- ############################################################################

data GenImgProjTransformer s =
     GenImgProjTransformer {
      giptSrcDs    :: Maybe (RODataset s)
    , giptDstDs    :: Maybe (RWDataset s)
    , giptSrcSrs   :: Maybe SpatialReference
    , giptDstSrs   :: Maybe SpatialReference
    , giptUseGCP   :: Bool
    , giptMaxError :: Double
    , giptOrder    :: Int
  }

instance Default (GenImgProjTransformer s) where
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
  createTransformerFun _ = return c_GDALGenImgProjTransform
  createTransformerArg GenImgProjTransformer{..} =
    liftM castPtr $
    throwIfError "GDALCreateGenImgProjTransformer" $
    withMaybeSRAsCString giptSrcSrs $ \sSr ->
    withMaybeSRAsCString giptDstSrs $ \dSr ->
      {#call CreateGenImgProjTransformer as ^#}
        (maybe nullDatasetH unDataset giptSrcDs)
        sSr
        (maybe nullDatasetH unDataset giptDstDs)
        dSr
        (fromBool giptUseGCP)
        (realToFrac giptMaxError)
        (fromIntegral giptOrder)

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform :: TransformerFun GenImgProjTransformer s


-- ############################################################################
-- GenImgProjTransformer2
-- ############################################################################

data GenImgProjTransformer2 s =
     GenImgProjTransformer2 {
      gipt2SrcDs    :: Maybe (RODataset s)
    , gipt2DstDs    :: Maybe (RWDataset s)
    , gipt2Options  :: OptionList
  }

instance Default (GenImgProjTransformer2 s) where
  def = GenImgProjTransformer2 {
          gipt2SrcDs   = Nothing
        , gipt2DstDs   = Nothing
        , gipt2Options = []
        }

instance Transformer GenImgProjTransformer2 where
  createTransformerFun _ = return c_GDALGenImgProjTransform2
  createTransformerArg GenImgProjTransformer2{..} =
    liftM castPtr $
    throwIfError "GDALCreateGenImgProjTransformer2" $
    withOptionList gipt2Options $ \opts ->
      {#call CreateGenImgProjTransformer2 as ^#}
        (maybe nullDatasetH unDataset gipt2SrcDs)
        (maybe nullDatasetH unDataset gipt2DstDs)
        opts

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform2 :: TransformerFun GenImgProjTransformer2 s

-- ############################################################################
-- GenImgProjTransformer3
-- ############################################################################

data GenImgProjTransformer3 s =
     GenImgProjTransformer3 {
      gipt3SrcSrs :: Maybe SpatialReference
    , gipt3DstSrs :: Maybe SpatialReference
    , gipt3SrcGt  :: Maybe Geotransform
    , gipt3DstGt  :: Maybe Geotransform
  }

instance Default (GenImgProjTransformer3 s) where
  def = GenImgProjTransformer3 {
          gipt3SrcSrs = Nothing
        , gipt3DstSrs = Nothing
        , gipt3SrcGt  = Nothing
        , gipt3DstGt  = Nothing
        }

instance Transformer GenImgProjTransformer3 where
  createTransformerFun _ = return c_GDALGenImgProjTransform3
  createTransformerArg GenImgProjTransformer3{..} =
    liftM castPtr $
    throwIfError "GDALCreateGenImgProjTransformer3" $
    withMaybeSRAsCString gipt3SrcSrs $ \sSr ->
    withMaybeSRAsCString gipt3DstSrs $ \dSr ->
    withMaybeGeotransformPtr gipt3SrcGt $ \sGt ->
    withMaybeGeotransformPtr gipt3DstGt $ \dGt ->
      {#call CreateGenImgProjTransformer3 as ^#}
        sSr (castPtr sGt) dSr (castPtr dGt)

withMaybeGeotransformPtr
  :: Maybe Geotransform -> (Ptr Geotransform -> IO a) -> IO a
withMaybeGeotransformPtr Nothing   f = f nullPtr
withMaybeGeotransformPtr (Just g) f = alloca $ \gp -> poke gp g >> f gp

foreign import ccall "gdal_alg.h &GDALGenImgProjTransform"
  c_GDALGenImgProjTransform3 :: TransformerFun GenImgProjTransformer3 s

-- ############################################################################
-- GDALRasterizeLayersBuf
-- ############################################################################

rasterizeLayersBuf
  :: forall t s a l. (GDALType a, Transformer t)
  => Size
  -> [ROLayer s l]
  -> SpatialReference
  -> Geotransform
  -> Maybe (t s)
  -> a
  -> a
  -> OptionList
  -> Maybe ProgressFun
  -> GDAL s (U.Vector (Value a))
rasterizeLayersBuf
  size layers srs geotransform mTransformer nodataValue burnValue options
  progressFun
  = liftIO $ rasterizeLayersBufIO size layers srs geotransform
               mTransformer nodataValue burnValue options progressFun

rasterizeLayersBufIO
  :: forall t s a l. (GDALType a, Transformer t)
  => Size
  -> [ROLayer s l]
  -> SpatialReference
  -> Geotransform
  -> Maybe (t s)
  -> a
  -> a
  -> OptionList
  -> Maybe ProgressFun
  -> IO (U.Vector (Value a))
rasterizeLayersBufIO
  size layers srs geotransform mTransformer nodataValue burnValue options
  progressFun = do
    ret <- withLockedLayerPtrs layers $ \layerPtrs ->
             withArrayLen layerPtrs $ \len lPtrPtr ->
             with geotransform $ \gt ->
             withMaybeSRAsCString (Just srs) $ \srsPtr ->
             withOptionList options $ \opts ->
             withTransformerAndArg mTransformer $ \trans tArg ->
             withProgressFun progressFun $ \pFun -> do
              mVec <- Stm.replicate (sizeLen size) nodataValue
              Stm.unsafeWith mVec $ \vecPtr ->
                 throwIfError_ "rasterizeLayersBuf" $
                   {#call GDALRasterizeLayersBuf as ^#}
                     (castPtr vecPtr) nx ny dt 0 0 (fromIntegral len)
                     lPtrPtr srsPtr (castPtr gt) (getTransformerFunPtr trans)
                     (castPtr tArg) bValue opts pFun nullPtr
              return mVec
    maybe (throwBindingException RasterizeStopped)
          (liftM (stToUValue . St.map toValue) . St.unsafeFreeze) ret
  where
    toValue v = if toCDouble v == ndValue then NoData else Value v
    dt        = fromEnumC (dataType (Proxy :: Proxy a))
    bValue    = toCDouble burnValue
    ndValue   = toCDouble nodataValue
    XY nx ny  = fmap fromIntegral size
