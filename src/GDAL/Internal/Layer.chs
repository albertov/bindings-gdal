{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

#include "bindings.h"
#include "gdal.h"
#include "ogr_api.h"

module GDAL.Internal.Layer (
    HasLayerTransaction
  , SQLDialect (..)
  , Layer (..)
  , LayerH (..)
  , ROLayer
  , RWLayer
  , OGR
  , OGRConduit
  , OGRSource
  , OGRSink
  , runOGR

  , canCreateMultipleGeometryFields

  , sourceLayer
  , sourceLayer_
  , conduitInsertLayer
  , conduitInsertLayer_
  , sinkInsertLayer
  , sinkInsertLayer_
  , sinkUpdateLayer

  , syncLayerToDisk

  , layerSpatialFilter
  , layerSpatialReference
  , setLayerSpatialFilter
  , setLayerSpatialFilterRect
  , clearLayerSpatialFilter

  , layerName
  , layerExtent
  , layerFeatureCount
  , layerFeatureDef

  , createFeature
  , createFeatureWithFid
  , createFeature_
  , getFeature
  , updateFeature
  , deleteFeature

  , unsafeToReadOnlyLayer

  , liftOGR
  , closeLayer
  , unLayer
  , layerHasCapability
  , nullLayerH
  , withSQLDialect
) where

{#context lib = "gdal" prefix = "OGR"#}

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Conduit ( Conduit, Sink, Source
                    , addCleanup, awaitForever, yield
                    , bracketP, catchC
                    , (=$=))
import qualified Data.Conduit.List as CL
import Data.Text (Text)

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Exception (SomeException)
import Control.Monad (liftM, when, void, (>=>), (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (
    MonadThrow(..)
  , MonadCatch
  , MonadMask
  )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool, fromBool)

import Foreign.Storable (Storable, peek)

import System.IO.Unsafe (unsafePerformIO)


{#import GDAL.Internal.OSR#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.OGRFeature#}
{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.CPLString#}
import GDAL.Internal.CPLError
import GDAL.Internal.Util
import GDAL.Internal.Types


-- A phantom-typed ST-like monad to make sure the layer does not escape its
-- scope
newtype OGR s l a = OGR { unOGR :: GDAL s a}

deriving instance Functor (OGR s l)
deriving instance Applicative (OGR s l)
deriving instance Monad (OGR s l)
deriving instance MonadIO (OGR s l)
deriving instance MonadThrow (OGR s l)
deriving instance MonadCatch (OGR s l)
deriving instance MonadMask (OGR s l)
deriving instance MonadBase IO (OGR s l)
deriving instance MonadResource (OGR s l)

instance MonadBaseControl IO (OGR s l) where
  type StM (OGR s l) a = a
  liftBaseWith runInBase = OGR $ do
    state <- getInternalState
    liftIO $ runInBase ((`runWithInternalState` state) . unOGR)
  restoreM = return

runOGR :: (forall l. OGR s l a ) -> GDAL s a
runOGR (OGR a) = a

liftOGR :: GDAL s a -> OGR s l a
liftOGR = OGR

type OGRConduit s l i o = Conduit i (OGR s l) o
type OGRSource s l o = Source (OGR s l) o
type OGRSink s l i = Sink i (OGR s l)

{#pointer LayerH newtype#}

deriving instance Storable LayerH
deriving instance Eq LayerH

nullLayerH :: LayerH
nullLayerH = LayerH nullPtr

newtype (Layer s l (t::AccessMode) a) = Layer (ReleaseKey, LayerH)

unLayer :: Layer s l t a -> LayerH
unLayer (Layer (_,l)) = l

closeLayer :: MonadIO m => Layer s l t a -> m ()
closeLayer (Layer (rk,_)) = release rk

type ROLayer s l = Layer s l ReadOnly
type RWLayer s l = Layer s l ReadWrite

unsafeToReadOnlyLayer :: RWLayer s l a -> ROLayer s l a
unsafeToReadOnlyLayer = coerce

data SQLDialect
  = DefaultDialect
  | SqliteDialect
  | OGRDialect
  deriving (Eq, Show, Enum)

withSQLDialect :: SQLDialect -> (CString -> IO a) -> IO a
withSQLDialect DefaultDialect = ($ nullPtr)
withSQLDialect SqliteDialect  = unsafeUseAsCString "SQLITE\0"
withSQLDialect OGRDialect     = unsafeUseAsCString "OGRSQL\0"



-- | GDAL < 1.11 only supports 1 geometry field and associates it to the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> maybeNewSpatialRefBorrowedHandle
          ({#call unsafe OGR_L_GetSpatialRef as ^#} p)
    <*> pure True

layerSpatialReference :: Layer s l a t -> GDAL s (Maybe SpatialReference)
layerSpatialReference
  = liftIO
  . maybeNewSpatialRefBorrowedHandle
  . {#call unsafe OGR_L_GetSpatialRef as ^#}
  . unLayer


syncLayerToDiskIO :: RWLayer s l a -> IO ()
syncLayerToDiskIO =
  checkOGRError "syncLayerToDisk" . {#call OGR_L_SyncToDisk as ^#} . unLayer

syncLayerToDisk :: RWLayer s l a -> GDAL s ()
syncLayerToDisk = liftIO . syncLayerToDiskIO

getLayerSchema :: LayerH -> IO FeatureDefnH
getLayerSchema = {#call OGR_L_GetLayerDefn as ^#}

createFeatureWithFidIO
  :: OGRFeature a => LayerH -> Maybe Fid -> a -> IO (Maybe Fid)
createFeatureWithFidIO pL fid feat = do
  pFd <- getLayerSchema pL
  featureToHandle pFd fid feat $ \pF -> do
    checkOGRError "CreateFeature" ({#call OGR_L_CreateFeature as ^#} pL pF)
    getFid pF

createFeatureWithFid
  :: OGRFeature a => RWLayer s l a -> Maybe Fid -> a -> GDAL s (Maybe Fid)
createFeatureWithFid layer fid =
  liftIO . createFeatureWithFidIO (unLayer layer) fid

createFeature :: OGRFeature a => RWLayer s l a -> a -> GDAL s Fid
createFeature layer =
  createFeatureWithFid layer Nothing >=>
    maybe (throwBindingException UnexpectedNullFid) return

createFeature_ :: OGRFeature a => RWLayer s l a -> a -> GDAL s ()
createFeature_ layer = void . createFeatureWithFid layer Nothing

updateFeature :: OGRFeature a => RWLayer s l a -> Fid -> a -> GDAL s ()
updateFeature layer fid feat = liftIO $ do
  pFd <- getLayerSchema pL
  featureToHandle pFd (Just fid) feat $
    checkOGRError "SetFeature" . {#call OGR_L_SetFeature as ^#} pL
  where pL = unLayer layer

getFeature :: OGRFeature a => Layer s l t a -> Fid -> GDAL s (Maybe a)
getFeature layer (Fid fid) = liftIO $ do
  when (not (pL `layerHasCapability` RandomRead)) $
    throwBindingException (UnsupportedLayerCapability RandomRead)
  fDef <- layerFeatureDefIO pL
  liftM (fmap snd) $ featureFromHandle fDef $
    checkGDALCall checkIt ({#call OGR_L_GetFeature as ^#} pL (fromIntegral fid))
  where
    checkIt (Just GDALException{gdalErrNum=AppDefined}) _ = Nothing
    checkIt   e _                                         = e
    pL = unLayer layer

deleteFeature :: Layer s l t a -> Fid -> GDAL s ()
deleteFeature layer (Fid fid) = liftIO $
  checkOGRError "DeleteFeature" $
    {#call OGR_L_DeleteFeature as ^#} (unLayer layer) (fromIntegral fid)

layerHasCapability :: LayerH -> LayerCapability -> Bool
layerHasCapability l c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_L_TestCapability as ^#} l)

layerName :: Layer s l t a -> GDAL s Text
layerName =
  liftIO . (peekEncodedCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

layerExtent :: Layer s l t a -> GDAL s EnvelopeReal
layerExtent l = liftIO $ alloca $ \pE -> do
  checkOGRError "GetExtent" ({#call OGR_L_GetExtent as ^#} (unLayer l) pE 1)
  peek pE

layerFeatureDef :: Layer s l t a -> GDAL s FeatureDef
layerFeatureDef = liftIO . layerFeatureDefIO . unLayer

layerFeatureDefIO :: LayerH -> IO FeatureDef
layerFeatureDefIO pL = do
  gfd <- layerGeomFieldDef pL
  getLayerSchema pL >>= featureDefFromHandle gfd

layerFeatureCount :: Layer s l t a -> Bool -> GDAL s (Maybe Int)
layerFeatureCount layer force = liftIO $ do
  c <- liftM fromIntegral $
        {#call OGR_L_GetFeatureCount as ^#} (unLayer layer) (fromBool force)
  if c<0 then return Nothing else return (Just c)

layerSpatialFilter :: Layer s l t a -> GDAL s (Maybe Geometry)
layerSpatialFilter l = liftIO $
  {#call unsafe OGR_L_GetSpatialFilter as ^#} (unLayer l) >>= maybeCloneGeometry

setLayerSpatialFilter :: Layer s l t a -> Geometry -> GDAL s ()
setLayerSpatialFilter l g = liftIO $
  withGeometry g $ {#call unsafe OGR_L_SetSpatialFilter as ^#} (unLayer l)

clearLayerSpatialFilter :: Layer s l t a -> GDAL s ()
clearLayerSpatialFilter l = liftIO $
  {#call unsafe OGR_L_SetSpatialFilter as ^#} (unLayer l) (nullPtr)

setLayerSpatialFilterRect :: Layer s l t a -> EnvelopeReal -> GDAL s ()
setLayerSpatialFilterRect l (Envelope (x0 :+: y0) (x1 :+: y1)) = liftIO $
  {#call unsafe OGR_L_SetSpatialFilterRect as ^#} (unLayer l)
    (realToFrac x0)
    (realToFrac y0)
    (realToFrac x1)
    (realToFrac y1)

sourceLayer
  :: (HasLayerTransaction t, OGRFeature a)
  => GDAL s (Layer s l t a)
  -> OGRSource s l (Maybe Fid, a)
sourceLayer alloc = layerTransaction alloc' loop
  where
    alloc' = do
      l <- alloc
      liftIO $ {#call OGR_L_ResetReading as ^#} (unLayer l)
      fDef <- layerFeatureDef l
      return (l, fDef)

    loop seed = do
      next <- liftIO $
              featureFromHandle (snd seed) $
              {#call OGR_L_GetNextFeature as ^#} (unLayer (fst seed))
      case next of
        Just v  -> yield v >> loop seed
        Nothing -> return ()

sourceLayer_
  :: (HasLayerTransaction t, OGRFeature a)
  => GDAL s (Layer s l t a) -> OGRSource s l a
sourceLayer_ = (=$= (CL.map snd)) . sourceLayer


conduitInsertLayer
  :: OGRFeature a
  => GDAL s (RWLayer s l a) -> OGRConduit s l (Maybe Fid, a) (Maybe Fid)
conduitInsertLayer = flip conduitFromLayer createIt
  where
    createIt (l, pFd) = awaitForever $ \(fid, feat) -> do
      fid' <- liftIO $ featureToHandle pFd fid feat $ \pF -> do
                checkOGRError "CreateFeature" $
                  {#call OGR_L_CreateFeature as ^#} (unLayer l) pF
                getFid pF
      yield fid'

conduitInsertLayer_
  :: OGRFeature a
  => GDAL s (RWLayer s l a) -> OGRConduit s l a (Maybe Fid)
conduitInsertLayer_ = (CL.map (\i->(Nothing,i)) =$=) . conduitInsertLayer

sinkInsertLayer
  :: OGRFeature a => GDAL s (RWLayer s l a) -> OGRSink s l (Maybe Fid, a) ()
sinkInsertLayer = (=$= CL.sinkNull) . conduitInsertLayer

sinkInsertLayer_
  :: OGRFeature a => GDAL s (RWLayer s l a) -> OGRSink s l a ()
sinkInsertLayer_ =
  ((=$= CL.sinkNull) . ((CL.map (\i->(Nothing,i))) =$=)) . conduitInsertLayer

sinkUpdateLayer
  :: OGRFeature a => GDAL s (RWLayer s l a) -> OGRSink s l (Fid, a) ()
sinkUpdateLayer = flip conduitFromLayer updateIt
  where
    updateIt (l, pFd) = awaitForever $ \(fid, feat) ->
      liftIO $ featureToHandle pFd (Just fid) feat $
        checkOGRError "SetFeature" . {#call OGR_L_SetFeature as ^#} (unLayer l)

class HasLayerTransaction (t :: AccessMode) where
  layerTransaction
    :: GDAL s (Layer s l t a, e)
    -> ((Layer s l t a, e) -> OGRConduit s l i o)
    -> OGRConduit s l i o

instance HasLayerTransaction ReadWrite where
  layerTransaction alloc inside = do
    state <- lift (liftOGR getInternalState)
    bracketP (alloc' state) free $ \ seed@(layer,_) -> do
      liftIO $ checkOGRError "StartTransaction" $
        {#call OGR_L_StartTransaction as ^#} (unLayer layer)
      addCleanup (\terminated -> when terminated (commit layer)) $
        inside seed `catchC`
          \(e :: SomeException) -> rollback layer >> throwM e
    where
      alloc' = runWithInternalState alloc
      free = closeLayer . fst
      commit layer = liftIO $ do
        checkOGRError "CommitTransaction" $
          {#call OGR_L_CommitTransaction as ^#} (unLayer layer)
        syncLayerToDiskIO layer

      rollback layer = liftIO $
        checkOGRError "RollbackTransaction" $
          {#call OGR_L_RollbackTransaction as ^#} (unLayer layer)

instance HasLayerTransaction ReadOnly where
  layerTransaction alloc inside = do
    state <- lift (liftOGR getInternalState)
    bracketP (alloc' state) free $ \ seed -> do
      liftIO $ checkOGRError "StartTransaction" $
        {#call OGR_L_StartTransaction as ^#} (unLayer (fst seed))
      inside seed
    where
      free = closeLayer . fst
      alloc' = runWithInternalState alloc


conduitFromLayer
  :: GDAL s (RWLayer s l a)
  -> ((RWLayer s l a, FeatureDefnH) -> OGRConduit s l i o)
  -> OGRConduit s l i o
conduitFromLayer alloc = layerTransaction alloc'
  where
    alloc' = do
      l <- alloc
      schema <- liftIO $ getLayerSchema (unLayer l)
      return (l, schema)


canCreateMultipleGeometryFields :: Bool
canCreateMultipleGeometryFields =
#if SUPPORTS_MULTI_GEOM_FIELDS
  True
#else
  False
#endif
