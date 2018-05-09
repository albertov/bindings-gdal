{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module GDAL.Internal.VRT (
  VRTDataset (..)
, VRTSourcedRasterBand (..)
, ScaleOffset(..)
, ScaleRatio(..)
, ImageReadFunc
, createVRT
, createVRTBand
, addSimpleSource
, addComplexSource
, addFuncSource
, vrtBandAsBand
) where

import GDAL.Internal.Types
import GDAL.Internal.Util
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.GDAL#}
import OGR (Envelope(..))

import Control.Monad (unless)
import Control.Exception (SomeException, catch)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Resource (allocate)
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.StablePtr
import qualified Data.Vector.Storable.Mutable as Stm

{#context lib = "gdal"  #}

#include "gdal.h"
#include "gdal_vrt.h"

{#pointer VRTDatasetH #}

newtype VRTDataset s (t :: AccessMode) = VRTDataset
  { unVrtDataset :: VRTDatasetH }

instance MajorObject (VRTDataset s) t where
  majorObject = MajorObjectH . castPtr . unVrtDataset

createVRT :: Size -> (VRTDataset s ReadWrite -> GDAL s ()) -> GDAL s (Dataset s a t)
createVRT (nx:+:ny) f = do
  (rk,vDs) <- allocate create_ close_
  unless (vDs/=nullPtr) (throwM (GDALBindingException NullDataset))
  f (VRTDataset vDs)
  liftIO ({#call VRTFlushCache as ^#} vDs)
  return (Dataset (Just rk, coerce vDs))
  where
    close_  = {#call GDALClose as ^#} . coerce
    create_ = {#call unsafe VRTCreate as ^#} (fromIntegral nx) (fromIntegral ny)



{#pointer VRTSourcedRasterBandH #}
newtype VRTSourcedRasterBand a (t :: AccessMode) = VRTSourcedRasterBand
  { unVrtBand :: VRTSourcedRasterBandH }

instance MajorObject (VRTSourcedRasterBand s) t where
  majorObject = MajorObjectH . castPtr . unVrtBand


createVRTBand
  :: GDALType a
  => DataType a -> VRTDataset s ReadWrite -> GDAL s (VRTSourcedRasterBand a ReadWrite)
createVRTBand dt (VRTDataset vDs) = liftIO $ do
  checkCPLError "VRTAddBand" $
    {#call unsafe VRTAddBand as ^#} vDs (fromEnumC (dataTypeK dt)) nullPtr
  n <- {#call unsafe GDALGetRasterCount as ^#} (coerce vDs)
  coerce <$> {#call unsafe GDALGetRasterBand as ^#} (coerce vDs) n



vrtBandAsBand :: forall t s a. GDALType a => VRTSourcedRasterBand a t -> Band s a t
vrtBandAsBand b = Band (coerce b, hsDataType (Proxy :: Proxy a))


newtype ScaleOffset = ScaleOffset Double
  deriving (Eq, Show)

instance Default ScaleOffset where def = ScaleOffset 0

newtype ScaleRatio = ScaleRatio Double
  deriving (Eq, Show)

instance Default ScaleRatio where def = ScaleRatio 1
  
addComplexSource
  :: GDALType a
  => VRTSourcedRasterBand b ReadWrite
  -> Envelope Int
  -> Band s a t
  -> Envelope Int
  -> Maybe a
  -> ScaleOffset
  -> ScaleRatio
  -> GDAL s ()
addComplexSource vBand dstEnv b srcEnv mNodata (ScaleOffset sOff) (ScaleRatio sRat) =
  liftIO $ checkCPLError "VRTAddComplexSource" $
  {#call unsafe VRTAddComplexSource as ^#}
    (unVrtBand vBand)
    (unBand b)
    srcX0
    srcY0
    (srcX1-srcX0)
    (srcY1-srcY0)
    dstX0
    dstY0
    (dstX1-dstX0)
    (dstY1-dstY0)
    (realToFrac sOff)
    (realToFrac sRat)
    noData
  where
    Envelope (srcX0 :+: srcY0) (srcX1 :+: srcY1) = fmap fromIntegral srcEnv
    Envelope (dstX0 :+: dstY0) (dstX1 :+: dstY1) = fmap fromIntegral dstEnv
    noData = maybe ({#const VRT_NODATA_UNSET#}) toCDouble mNodata

addSimpleSource
  :: GDALType a
  => VRTSourcedRasterBand b ReadWrite
  -> Envelope Int
  -> Band s a t
  -> Envelope Int
  -> Resampling
  -> Maybe a
  -> GDAL s ()
addSimpleSource vBand dstEnv b srcEnv res mNodata =
  liftIO $ withResampling res $ \cRes ->
  checkCPLError "VRTAddSimpleSource" $
  {#call unsafe VRTAddSimpleSource as ^#}
    (unVrtBand vBand)
    (unBand b)
    srcX0
    srcY0
    (srcX1-srcX0)
    (srcY1-srcY0)
    dstX0
    dstY0
    (dstX1-dstX0)
    (dstY1-dstY0)
    cRes
    noData
  where
    Envelope (srcX0 :+: srcY0) (srcX1 :+: srcY1) = fmap fromIntegral srcEnv
    Envelope (dstX0 :+: dstY0) (dstX1 :+: dstY1) = fmap fromIntegral dstEnv
    noData = maybe ({#const VRT_NODATA_UNSET#}) toCDouble mNodata


type ImageReadFunc a = Envelope Int -> Stm.IOVector a -> IO ()

addFuncSource
  :: (SizeOf a ~ 8, GDALType a)
  => VRTSourcedRasterBand a ReadWrite
  -> ImageReadFunc a
  -> GDAL s ()
addFuncSource vBand fun = do
  funPtr <- snd <$> allocate (newStablePtr fun) freeStablePtr
  liftIO $ checkCPLError "VRTAddFuncSource" $
    {#call unsafe VRTAddFuncSource as ^#}
    (unVrtBand vBand)
    hs_gdal_image_read_func_ptr
    (castStablePtrToPtr funPtr)
    -- Hardcode VRT_NODATA_UNSET because gdal seems to ignore it.
    -- The ImageReadFunc should return the appropiate nodata value as defined
    -- in the band if needed
    ({#const VRT_NODATA_UNSET#})
      

type CImageReadFunc a = Ptr () -> CInt -> CInt -> CInt -> CInt -> Ptr a -> IO CInt

hs_gdal_image_read_func :: forall a. CImageReadFunc a
hs_gdal_image_read_func funRef x0 y0 nx ny pData = fromEnumC <$> (do
  let env = fromIntegral <$> Envelope (x0:+:y0) ((x0+nx):+:(y0+ny))
  fun :: ImageReadFunc a <- deRefStablePtr (castPtrToStablePtr funRef)
  fun env =<< Stm.MVector (fromIntegral (nx*ny)) <$> newForeignPtr_ pData
  pure CE_None
  ) `catch` (\(e::SomeException) -> do
  cplError CE_Failure AppDefined (T.pack (show e))
  pure CE_Failure
  )

foreign export ccall hs_gdal_image_read_func :: CImageReadFunc a
foreign import ccall "&hs_gdal_image_read_func"
  hs_gdal_image_read_func_ptr :: FunPtr (CImageReadFunc a)
