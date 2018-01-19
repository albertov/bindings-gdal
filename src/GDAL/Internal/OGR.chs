{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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


module GDAL.Internal.OGR (
    DataSource
  , DataSourceH
  , SQLDialect (..)
  , Layer
  , LayerH (..)
  , RODataSource
  , RWDataSource
  , ROLayer
  , RWLayer
  , Driver
  , OGR
  , OGRConduit
  , OGRSource
  , OGRSink
  , runOGR

  , openReadOnly
  , openReadWrite
  , create
  , createMem
  , canCreateMultipleGeometryFields

  , dataSourceName

  , createLayer
  , createLayerWithDef

  , getLayer
  , getLayerByName

  , sourceLayer
  , sourceLayer_
  , conduitInsertLayer
  , conduitInsertLayer_
  , sinkInsertLayer
  , sinkInsertLayer_
  , sinkUpdateLayer

  , executeSQL

  , syncLayerToDisk
  , syncToDisk

  , layerSpatialFilter
  , layerSpatialReference
  , setLayerSpatialFilter
  , setLayerSpatialFilterRect
  , clearLayerSpatialFilter

  , layerCount
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

  , registerAll
  , cleanupAll

  , liftOGR
  , closeLayer
  , unDataSource
  , unLayer
  , layerHasCapability
  , driverHasCapability
  , dataSourceHasCapability
  , nullLayerH
) where

{#context lib = "gdal" prefix = "OGR"#}

import Data.ByteString.Char8 (ByteString, useAsCString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector as V

import Control.Monad (liftM, when, (<=<))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Utils (toBool)


import System.IO.Unsafe (unsafePerformIO)


{#import GDAL.Internal.OSR#}
{#import GDAL.Internal.Layer#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.OGRFeature#}
{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.CPLString#}
import GDAL.Internal.Common
import GDAL.Internal.CPLError
import GDAL.Internal.Util
import GDAL.Internal.Types

{#fun RegisterAll as ^ {} -> `()'  #}
{#fun CleanupAll  as ^ {} -> `()'  #}

{#pointer OGRSFDriverH as DriverH #}


{#pointer DataSourceH newtype#}
newtype DataSource s (t::AccessMode) =
  DataSource { unDataSource :: DataSourceH }

deriving instance Eq DataSourceH

nullDataSourceH :: DataSourceH
nullDataSourceH = DataSourceH nullPtr

type RODataSource s = DataSource s ReadOnly
type RWDataSource s = DataSource s ReadWrite



{#enum define OGRAccess
  { FALSE  as OGR_ReadOnly
  , TRUE   as OGR_Update
  } deriving (Eq, Show) #}


openReadOnly :: String -> GDAL s (RODataSource s)
openReadOnly p = openWithMode OGR_ReadOnly p

openReadWrite :: String -> GDAL s (RWDataSource s)
openReadWrite p = openWithMode OGR_Update p

openWithMode :: OGRAccess -> String -> GDAL s (DataSource s t)
openWithMode m p =
  newDataSourceHandle $ withCString p $ \p' ->
    checkGDALCall checkIt ({#call OGROpen as ^#} p' (fromEnumC m) nullPtr)
  where
    checkIt e p' | p'==nullDataSourceH = Just (fromMaybe dflt e)
    checkIt e _                        = e
    dflt = GDALException CE_Failure OpenFailed "OGROpen"


newDataSourceHandle
  :: IO DataSourceH -> GDAL s (DataSource s t)
newDataSourceHandle act = liftM snd $ allocate alloc free
  where
    alloc = liftM DataSource act
    free ds
      | dsPtr /= nullDataSourceH = {#call OGR_DS_Destroy as ^#} dsPtr
      | otherwise                = return ()
      where dsPtr = unDataSource ds

newtype Driver =
  Driver ByteString deriving (Eq, IsString, Typeable)

instance Show Driver where
  show (Driver s) = show s

create :: Driver -> String -> OptionList -> GDAL s (RWDataSource s)
create driverName name opts = newDataSourceHandle $ do
  pDr <- driverByName driverName
  withCString name $ \pName ->
    checkGDALCall checkIt
      (withOptionList opts $ {#call OGR_Dr_CreateDataSource as ^#} pDr pName)
  where
    checkIt e p' | p'==nullDataSourceH = Just (fromMaybe dflt e)
    checkIt e _                        = e
    dflt = GDALException CE_Failure OpenFailed "OGR_Dr_CreateDataSource"

createMem :: OptionList -> GDAL s (RWDataSource s)
createMem = create "Memory" ""

driverByName :: Driver -> IO DriverH
driverByName (Driver name) = useAsCString name $ \pName -> do
  drv <- {#call unsafe GetDriverByName as ^#} pName
  if drv == nullPtr
    then throwBindingException (UnknownOGRDriver name)
    else return drv

createLayer
  :: forall s l a. OGRFeatureDef a
  => RWDataSource s -> ApproxOK -> OptionList -> GDAL s (RWLayer s l a)
createLayer ds = createLayerWithDef ds (featureDef (Proxy :: Proxy a))



createLayerWithDef
  :: RWDataSource s -> FeatureDef -> ApproxOK -> OptionList
  -> GDAL s (RWLayer s l a)
createLayerWithDef ds FeatureDef{..} approxOk opts =
  liftM Layer $
  flip allocate (const (return ())) $
  useAsEncodedCString fdName $ \pName ->
  withMaybeSpatialReference (gfdSrs fdGeom) $ \pSrs ->
  withOptionList opts $ \pOpts -> do
    pL <- checkGDALCall checkIt $
            {#call OGR_DS_CreateLayer as ^#} pDs pName pSrs gType pOpts
    V.forM_ fdFields $ \(n,f) -> withFieldDefnH n f $ \pFld ->
      checkOGRError "CreateField" $
        {#call unsafe OGR_L_CreateField as ^#} pL pFld iApproxOk
    when (not (V.null fdGeoms)) $
      if supportsMultiGeomFields pL
        then
#if SUPPORTS_MULTI_GEOM_FIELDS
          V.forM_ fdGeoms $ \(n,f) -> withGeomFieldDefnH n f $ \pGFld ->
            {#call unsafe OGR_L_CreateGeomField as ^#} pL pGFld iApproxOk
#else
          error "should never reach here"
#endif
        else throwBindingException MultipleGeomFieldsNotSupported
    return pL
  where
    supportsMultiGeomFields pL =
      layerHasCapability pL CreateGeomField &&
      dataSourceHasCapability pDs CreateGeomFieldAfterCreateLayer
    iApproxOk = fromEnumC approxOk
    pDs   = unDataSource ds
    gType = fromEnumC (gfdType fdGeom)
    checkIt e p' | p'==nullLayerH = Just (fromMaybe dflt e)
    checkIt e _                   = e
    dflt = GDALBindingException NullLayer


syncToDisk :: RWDataSource s -> GDAL s ()
syncToDisk =
  liftIO .
  checkOGRError "syncToDisk" .
  {#call OGR_DS_SyncToDisk as ^#} .
  unDataSource


getLayer :: Int -> DataSource s t -> GDAL s (Layer s l t a)
getLayer ix ds =
  liftM Layer $
  flip allocate (const (return ())) $
  checkGDALCall checkIt $
    {#call OGR_DS_GetLayer as ^#} (unDataSource ds) (fromIntegral ix)
  where
    checkIt e p' | p'==nullLayerH = Just (fromMaybe dflt e)
    checkIt e _                   = e
    dflt = GDALBindingException (InvalidLayerIndex ix)


getLayerByName :: Text -> DataSource s t -> GDAL s (Layer s l t a)
getLayerByName name ds =
  liftM Layer $
  flip allocate (const (return ())) $
  checkGDALCall checkIt $
  useAsEncodedCString name $
  {#call OGR_DS_GetLayerByName as ^#} (unDataSource ds)
  where
    checkIt e p' | p'==nullLayerH = Just (fromMaybe dflt e)
    checkIt e _                   = e
    dflt = GDALBindingException (InvalidLayerName name)



layerCount :: DataSource s t -> GDAL s Int
layerCount = liftM fromIntegral
           . liftIO . {#call OGR_DS_GetLayerCount as ^#} . unDataSource

dataSourceName :: DataSource s t -> GDAL s String
dataSourceName =
  liftIO . (peekCString <=< {#call OGR_DS_GetName as ^#} . unDataSource)



executeSQL
  :: OGRFeature a
  => SQLDialect -> Text -> Maybe Geometry -> RODataSource s
  -> GDAL s (ROLayer s l a)
executeSQL dialect query mSpatialFilter ds =
  liftM Layer $ allocate execute freeIfNotNull
  where
    execute =
      checkGDALCall checkit $
      withMaybeGeometry mSpatialFilter $ \pF ->
      useAsEncodedCString query $ \pQ ->
      withSQLDialect dialect $ {#call OGR_DS_ExecuteSQL as ^#} pDs pQ pF

    freeIfNotNull pL
      | pL /= nullLayerH = {#call unsafe OGR_DS_ReleaseResultSet as ^#} pDs pL
      | otherwise        = return ()

    pDs = unDataSource ds
    checkit (Just (GDALException{gdalErrNum=AppDefined, gdalErrMsg=msg})) _ =
      Just (GDALBindingException (SQLQueryError msg))
    checkit Nothing p | p==nullLayerH =
      Just (GDALBindingException NullLayer)
    checkit e p | p==nullLayerH = e
    checkit _ _                 = Nothing


driverHasCapability :: DriverH -> DriverCapability -> Bool
driverHasCapability d c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_Dr_TestCapability as ^#} d)


dataSourceHasCapability :: DataSourceH -> DataSourceCapability -> Bool
dataSourceHasCapability d c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_DS_TestCapability as ^#} d)
