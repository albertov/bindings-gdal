{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

#include "bindings.h"

module GDAL.Internal.OGR (
    DataSource
  , DataSourceH
  , SQLDialect (..)
  , ApproxOK (..)
  , Layer
  , LayerH (..)
  , FeatureSource
  , FeatureCreator
  , FeatureUpdater
  , RODataSource
  , RWDataSource
  , ROLayer
  , RWLayer
  , Driver

  , openReadOnly
  , openReadWrite
  , create
  , createMem
  , canCreateMultipleGeometryFields

  , datasourceName
  , executeSQL

  , createLayer
  , createLayerWithDef

  , getLayer
  , getLayerByName

  , getFeatureSource
  , getFeatureSourceByName

  , getFeatureCreator
  , getFeatureCreatorByName

  , getFeatureUpdater
  , getFeatureUpdaterByName

  , syncToDisk

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
  , layerName
  , layerFeatureDef

  , createFeature
  , createFeatureWithFid
  , createFeature_
  , getFeature
  , setFeature
  , deleteFeature

  , registerAll
  , cleanupAll

  , unDataSource
  , unLayer
  , layerHasCapability
  , driverHasCapability
  , dataSourceHasCapability
  , sourceFromLayer
  , nullLayerH
) where

{#context lib = "gdal" prefix = "OGR"#}

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Conduit
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Vector as V

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, when, void, (>=>), (<=<))
import Control.Monad.Catch(throwM, catch, catchJust)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..), CLong(..))
#if GDAL_VERSION_MAJOR >= 2
import Foreign.C.Types (CLLong(..))
#endif
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils (toBool)

import Foreign.Storable (Storable)

import System.IO.Unsafe (unsafePerformIO)


{#import GDAL.Internal.OSR#}
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.OGRFeature#}
{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.CPLString#}
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.Util
import GDAL.Internal.Types

#include "ogr_api.h"

{#fun RegisterAll as ^ {} -> `()'  #}
{#fun CleanupAll  as ^ {} -> `()'  #}

{#pointer OGRSFDriverH as DriverH newtype#}
deriving instance Eq DriverH

nullDriverH :: DriverH
nullDriverH = DriverH nullPtr


{#pointer DataSourceH newtype#}
newtype DataSource s (t::AccessMode) =
  DataSource { unDataSource :: DataSourceH }

deriving instance Eq DataSourceH

nullDataSourceH :: DataSourceH
nullDataSourceH = DataSourceH nullPtr

type RODataSource s = DataSource s ReadOnly
type RWDataSource s = DataSource s ReadWrite

{#pointer LayerH newtype#}

deriving instance Storable LayerH
deriving instance Eq LayerH

nullLayerH :: LayerH
nullLayerH = LayerH nullPtr

newtype (Layer s (t::AccessMode) a) =
  Layer { unLayer :: LayerH }

type ROLayer s = Layer s ReadOnly
type RWLayer s = Layer s ReadWrite

type FeatureSource s a = GDALSource s (Maybe Fid, a)
type FeatureCreator s a = GDALConduit s (Maybe Fid, a) (Maybe Fid)
type FeatureUpdater s a = GDALSink s (Fid, a) ()

{#enum define OGRAccess
  { FALSE  as OGR_ReadOnly
  , TRUE   as OGR_Update
  } deriving (Eq, Show) #}


openReadOnly :: String -> GDAL s (RODataSource s)
openReadOnly p = openWithMode OGR_ReadOnly p

openReadWrite :: String -> GDAL s (RWDataSource s)
openReadWrite p = openWithMode OGR_Update p

openWithMode :: OGRAccess -> String -> GDAL s (DataSource s t)
openWithMode m p = do
  ptr <- liftIO $ withCString p $ \p' ->
          {#call OGROpen as ^#} p' (fromEnumC m) nullPtr
  newDataSourceHandle ptr `catch` (\NullDataSource ->
    throwM (GDALException CE_Failure OpenFailed "OGROpen returned a NULL ptr"))


newDataSourceHandle :: DataSourceH -> GDAL s (DataSource s t)
newDataSourceHandle p
  | p==nullDataSourceH = throwBindingException NullDataSource
  | otherwise          = do
      registerFinalizer (void ({#call unsafe OGR_DS_Destroy as ^#} p))
      return (DataSource p)

type Driver = String

create :: Driver -> String -> OptionList -> GDAL s (RWDataSource s)
create driverName name options = newDataSourceHandle <=<
  liftIO $ do
    pDr <- driverByName driverName
    withCString name $ \pName ->
      withOptionList options $ {#call OGR_Dr_CreateDataSource as ^#} pDr pName

createMem :: OptionList -> GDAL s (RWDataSource s)
createMem = create "Memory" ""

driverByName :: String -> IO DriverH
driverByName name = withCString name $ \pName -> do
  drv <- {#call unsafe GetDriverByName as ^#} pName
  if drv == nullDriverH
    then throwBindingException (UnknownDriver name)
    else return drv

{#enum define ApproxOK
  { TRUE as ApproxOK
  , FALSE as StrictOK
  } deriving (Eq, Show) #}

createLayer
  :: forall s a. OGRFeatureDef a
  => RWDataSource s -> ApproxOK -> OptionList -> GDAL s (RWLayer s a)
createLayer ds = createLayerWithDef ds (featureDef (Proxy :: Proxy a))



createLayerWithDef
  :: RWDataSource s -> FeatureDef -> ApproxOK -> OptionList
  -> GDAL s (RWLayer s a)
createLayerWithDef ds FeatureDef{..} approxOk options =
  throwIfError "createLayerWithDef" $
  liftIO $
  useAsEncodedCString fdName $ \pName ->
  withMaybeSpatialReference (gfdSrs fdGeom) $ \pSrs ->
  withOptionList options $ \pOpts -> do
    pL <- {#call OGR_DS_CreateLayer as ^#} pDs pName pSrs gType pOpts
    when (pL == nullLayerH) (throwBindingException NullLayer)
    V.forM_ fdFields $ \(n,f) -> withFieldDefnH n f $ \pFld ->
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
    return (Layer pL)
  where
    supportsMultiGeomFields pL =
      layerHasCapability pL CreateGeomField &&
      dataSourceHasCapability pDs CreateGeomFieldAfterCreateLayer
    iApproxOk = fromEnumC approxOk
    pDs   = unDataSource ds
    gType = fromEnumC (gfdType fdGeom)

getLayerSchema :: LayerH -> IO FeatureDefnH
getLayerSchema = {#call OGR_L_GetLayerDefn as ^#}

createFeatureWithFidIO
  :: OGRFeature a => LayerH -> Maybe Fid -> a -> IO (Maybe Fid)
createFeatureWithFidIO pL fid feat = do
  pFd <- getLayerSchema pL
  featureToHandle pFd fid feat $ \pF -> do
    checkOGRErr ({#call OGR_L_CreateFeature as ^#} pL pF)
    getFid pF

createFeatureWithFid
  :: OGRFeature a => RWLayer s a -> Maybe Fid -> a -> GDAL s (Maybe Fid)
createFeatureWithFid layer fid =
  liftIO . createFeatureWithFidIO (unLayer layer) fid

createFeature :: OGRFeature a => RWLayer s a -> a -> GDAL s Fid
createFeature layer =
  createFeatureWithFid layer Nothing >=>
    maybe (throwBindingException UnexpectedNullFid) return

createFeature_ :: OGRFeature a => RWLayer s a -> a -> GDAL s ()
createFeature_ layer = void . createFeatureWithFid layer Nothing

checkOGRErr :: IO CInt -> IO ()
checkOGRErr = checkReturns_ (==fromEnumC None)

setFeature :: OGRFeature a => RWLayer s a -> Fid -> a -> GDAL s ()
setFeature layer fid feat = liftIO $ do
  pFd <- getLayerSchema pL
  featureToHandle pFd (Just fid) feat $
    checkOGRErr . {#call OGR_L_SetFeature as ^#} pL
  where pL = unLayer layer

getFeature :: OGRFeature a => Layer s t a -> Fid -> GDAL s (Maybe a)
getFeature layer (Fid fid) = liftIO $ do
  when (not (pL `layerHasCapability` RandomRead)) $
    throwBindingException (UnsupportedLayerCapability RandomRead)
  fDef <- layerFeatureDefIO pL
  liftM (fmap snd) $ featureFromHandle fDef $
    {#call OGR_L_GetFeature as ^#} pL (fromIntegral fid)
  where pL = unLayer layer

deleteFeature :: Layer s t a -> Fid -> GDAL s ()
deleteFeature layer (Fid fid) = liftIO $
  checkOGRErr $
    {#call OGR_L_DeleteFeature as ^#} (unLayer layer) (fromIntegral fid)

canCreateMultipleGeometryFields :: Bool
canCreateMultipleGeometryFields =
#if SUPPORTS_MULTI_GEOM_FIELDS
  True
#else
  False
#endif

syncToDiskIO :: LayerH -> IO ()
syncToDiskIO = checkOGRErr . {#call OGR_L_SyncToDisk as ^#}


syncToDisk :: RWLayer s a -> GDAL s ()
syncToDisk = liftIO . syncToDiskIO . unLayer


getLayerIO :: Int -> DataSource s t -> IO LayerH
getLayerIO ix ds = do
  pL <- {#call OGR_DS_GetLayer as ^#} (unDataSource ds) (fromIntegral ix)
  when (pL == nullLayerH) (throwBindingException (InvalidLayerIndex ix))
  return pL

getLayer :: Int -> DataSource s t -> GDAL s (Layer s t a)
getLayer ix = liftIO . liftM Layer . getLayerIO ix

getFeatureSource
  :: OGRFeature a => Int -> DataSource s t -> FeatureSource s a
getFeatureSource ix ds =
  sourceFromLayer (getLayerIO ix ds >>= \p -> return (p,return()))

getFeatureUpdater
  :: OGRFeature a => Int -> RWDataSource s -> FeatureUpdater s a
getFeatureUpdater ix = featureUpdaterFromLayer . getLayerIO ix

getFeatureCreator
  :: OGRFeature a => Int -> RWDataSource s -> FeatureCreator s a
getFeatureCreator ix = featureCreatorFromLayer . getLayerIO ix

getLayerByNameIO :: Text -> DataSource s t -> IO LayerH
getLayerByNameIO name ds = useAsEncodedCString name $ \lName -> do
  pL <- {#call OGR_DS_GetLayerByName as ^#} (unDataSource ds) lName
  when (pL==nullLayerH) (throwBindingException (InvalidLayerName name))
  return pL

getLayerByName :: Text -> DataSource s t -> GDAL s (Layer s t a)
getLayerByName name = liftIO . liftM Layer . getLayerByNameIO name

getFeatureSourceByName
  :: OGRFeature a => Text -> DataSource s t -> FeatureSource s a
getFeatureSourceByName name ds =
  sourceFromLayer (getLayerByNameIO name ds >>= \p -> return (p,return()))

getFeatureUpdaterByName
  :: OGRFeature a => Text -> RWDataSource s -> FeatureUpdater s a
getFeatureUpdaterByName name = featureUpdaterFromLayer . getLayerByNameIO name

getFeatureCreatorByName
  :: OGRFeature a => Text -> RWDataSource s -> FeatureCreator s a
getFeatureCreatorByName name = featureCreatorFromLayer . getLayerByNameIO name


sourceFromLayer
  :: forall s a. OGRFeature a => IO (LayerH, IO ()) -> FeatureSource s a
sourceFromLayer getHandle = bracketP initialize finish getNext
  where
    initialize = do
      (pL, finalizer) <- getHandle
      when (pL == nullLayerH) (throwBindingException NullLayer)
      fDef <- layerFeatureDefIO pL
      void $ {#call OGR_L_StartTransaction as ^#} pL
      void $ {#call OGR_L_ResetReading as ^#}  pL
      return (pL, finalizer, fDef)

    getNext info@(pL, _, fDef) = do
      next <- liftIO $
              featureFromHandle fDef $
              {#call OGR_L_GetNextFeature as ^#} pL
      case next of
        Just v  -> yield v >> getNext info
        Nothing -> return ()

    finish (pL, finalizer, _) =
      {#call OGR_L_RollbackTransaction as ^#} pL >> finalizer

conduitFromLayer
  :: IO LayerH -> ((LayerH, FeatureDefnH) -> GDALConduit s i o)
  -> GDALConduit s i o
conduitFromLayer getHandle = bracketP initialize finish
  where
    initialize = do
      pL <- getHandle
      when (pL == nullLayerH) (throwBindingException NullLayer)
      schema <- getLayerSchema pL
      void $ {#call OGR_L_StartTransaction as ^#} pL
      return (pL, schema)

    finish (pL, _) = do
      void $ {#call OGR_L_CommitTransaction as ^#} pL
      syncToDiskIO pL

featureCreatorFromLayer
  :: OGRFeature a => IO LayerH -> FeatureCreator s a
featureCreatorFromLayer = flip conduitFromLayer createIt
  where
    createIt (pL, pFd) = awaitForever $ \(fid, feat) -> do
      fid' <- liftIO $ featureToHandle pFd fid feat $ \pF -> do
                checkOGRErr ({#call OGR_L_CreateFeature as ^#} pL pF)
                getFid pF
      yield fid'

featureUpdaterFromLayer
  :: OGRFeature a => IO LayerH -> FeatureUpdater s a
featureUpdaterFromLayer = flip conduitFromLayer updateIt
  where
    updateIt (pL, pFd) = awaitForever $ \(fid, feat) ->
      liftIO $ featureToHandle pFd (Just fid) feat $
        checkOGRErr . {#call OGR_L_SetFeature as ^#} pL

-- | GDAL < 1.11 only supports 1 geometry field and associates it to the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> ({#call unsafe OGR_L_GetSpatialRef as ^#} p >>=
          maybeNewSpatialRefBorrowedHandle)
    <*> pure True

layerCount :: DataSource s t -> GDAL s Int
layerCount = liftM fromIntegral
           . liftIO . {#call OGR_DS_GetLayerCount as ^#} . unDataSource

datasourceName :: DataSource s t -> GDAL s String
datasourceName =
  liftIO . (peekCString <=< {#call unsafe OGR_DS_GetName as ^#} . unDataSource)


data SQLDialect
  = DefaultDialect
  | SqliteDialect
  | OGRDialect
  deriving (Eq, Show, Enum)

withSQLDialect :: SQLDialect -> (CString -> IO a) -> IO a
withSQLDialect DefaultDialect = ($ nullPtr)
withSQLDialect SqliteDialect  = unsafeUseAsCString "SQLITE"
withSQLDialect OGRDialect     = unsafeUseAsCString "OGRSQL"

executeSQL
  :: OGRFeature a
  => SQLDialect -> Text -> Maybe Geometry -> RODataSource s
  -> FeatureSource s a
executeSQL dialect query mSpatialFilter ds = sourceFromLayer $ do
  p <- catchJust selectExc execute (throwBindingException . SQLQueryError)
  return (p, {#call unsafe OGR_DS_ReleaseResultSet as ^#} (unDataSource ds) p)
  where
    execute =
      throwIfError "executeSQL" $
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      useAsEncodedCString query $ \sQuery ->
      withSQLDialect dialect $
        {#call OGR_DS_ExecuteSQL as ^#} (unDataSource ds) sQuery sFilter
    selectExc GDALException{gdalErrNum=AppDefined,..} = Just gdalErrMsg
    selectExc _                                       = Nothing

layerName :: Layer s t a -> GDAL s Text
layerName =
  liftIO . (peekEncodedCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

layerFeatureDef :: Layer s t a -> GDAL s FeatureDef
layerFeatureDef = liftIO . layerFeatureDefIO . unLayer

layerFeatureDefIO :: LayerH -> IO FeatureDef
layerFeatureDefIO pL = do
  gfd <- layerGeomFieldDef pL
  getLayerSchema pL >>= featureDefFromHandle gfd


getSpatialFilter :: Layer s t a -> GDAL s (Maybe Geometry)
getSpatialFilter l = liftIO $ do
  p <- {#call unsafe OGR_L_GetSpatialFilter as ^#} (unLayer l)
  if p == nullPtr
    then return Nothing
    else liftM Just (cloneGeometry p)

setSpatialFilter :: Layer s t a -> Geometry -> GDAL s ()
setSpatialFilter l g = liftIO $
  withGeometry g $ {#call unsafe OGR_L_SetSpatialFilter as ^#} (unLayer l)

driverHasCapability :: DriverH -> DriverCapability -> Bool
driverHasCapability d c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_Dr_TestCapability as ^#} d)

layerHasCapability :: LayerH -> LayerCapability -> Bool
layerHasCapability l c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_L_TestCapability as ^#} l)

dataSourceHasCapability :: DataSourceH -> DataSourceCapability -> Bool
dataSourceHasCapability d c = unsafePerformIO $ do
  withCString (show c)
    (liftM toBool . {#call unsafe OGR_DS_TestCapability as ^#} d)
