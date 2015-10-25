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
  , LayerSource
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
  , syncToDisk

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
  , layerName
  , layerFeatureDef

  , createFeature
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

import Data.Conduit
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Vector as V

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, when, void, (<=<))
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
      registerFinalizer (void ({#call unsafe ReleaseDataSource as ^#} p))
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
      checkOGRErr $ {#call OGR_L_CreateField as ^#} pL pFld iApproxOk
    when (not (V.null fdGeoms)) $
      if supportsMultiGeomFields pL
        then
#if SUPPORTS_MULTI_GEOM_FIELDS
          V.forM_ fdGeoms $ \(n,f) -> withGeomFieldDefnH n f $ \pGFld ->
            {#call OGR_L_CreateGeomField as ^#} pL pGFld iApproxOk
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

createFeature :: OGRFeature a => RWLayer s a -> a -> GDAL s Fid
createFeature layer feat = liftIO $ do
  pFd <- getLayerSchema pL
  featureToHandle pFd Nothing feat $ \pF -> do
    checkOGRErr ({#call OGR_L_CreateFeature as ^#} pL pF)
    getFid pF >>= maybe (throwBindingException UnexpectedNullFid) return
  where pL = unLayer layer

createFeature_ :: OGRFeature a => RWLayer s a -> a -> GDAL s ()
createFeature_ layer feat =
  catchJust (\case {UnexpectedNullFid -> Just (); _ -> Nothing})
            (void (createFeature layer feat))
            return

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

syncToDisk :: RWLayer s a -> GDAL s ()
syncToDisk =
  liftIO . checkOGRErr . {#call OGR_L_SyncToDisk as ^#} . unLayer

getLayer :: Int -> DataSource s t -> GDAL s (Layer s t a)
getLayer ix ds = liftIO $ do
  pL <- {#call OGR_DS_GetLayer as ^#} p i
  when (pL == nullLayerH) (throwBindingException (InvalidLayerIndex ix))
  return (Layer pL)
  where
    p = unDataSource ds
    i = fromIntegral ix


getLayerByName :: Text -> DataSource s t -> GDAL s (Layer s t a)
getLayerByName layer ds = liftIO $ useAsEncodedCString layer $ \lName -> do
  pL <- {#call OGR_DS_GetLayerByName as ^#} (unDataSource ds) lName
  when (pL==nullLayerH) (throwBindingException (InvalidLayerName layer))
  return (Layer pL)

type LayerSource s a = Source (GDAL s) a

sourceFromLayer
  :: forall s a. OGRFeature a
  => IO LayerH -> LayerSource s (Maybe Fid, a)
sourceFromLayer getHandle = bracketP initialize cleanUp getFeatures
  where
    initialize = do
      pL <- getHandle
      when (pL == nullLayerH) (throwBindingException NullLayer)
      void $ {#call OGR_L_StartTransaction as ^#} pL
      void $ {#call unsafe OGR_L_ResetReading as ^#}  pL
      fDef <- layerFeatureDefIO pL
      return (pL, fDef)

    getFeatures info@(pL, fDef) = do
      next <- liftIO $
              featureFromHandle fDef $
              {#call OGR_L_GetNextFeature as ^#} pL
      case next of
        Just v  -> yield v >> getFeatures info
        Nothing -> return ()

    cleanUp = void . {#call OGR_L_RollbackTransaction as ^#} . fst


-- | GDAL < 1.11 only supports 1 geometry field and associates it the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> ({#call unsafe OGR_L_GetSpatialRef as ^#} p >>=
          maybeNewSpatialRefBorrowedHandle)
    <*> pure True

layerCount :: DataSource s t -> GDAL s Int
layerCount = liftM fromIntegral
           . liftIO . {#call unsafe OGR_DS_GetLayerCount as ^#} . unDataSource

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
withSQLDialect SqliteDialect  = withCString "SQLITE"
withSQLDialect OGRDialect     = withCString "OGRSQL"

executeSQL
  :: SQLDialect -> Text -> Maybe Geometry -> RODataSource s
  -> GDAL s (ROLayer s a)
executeSQL dialect query mSpatialFilter ds = do
  p <- catchJust selectExc execute (throwBindingException . SQLQueryError)
  when (p==nullLayerH) $ throwBindingException NullLayer
  registerFinalizer $
    {#call unsafe OGR_DS_ReleaseResultSet as ^#} (unDataSource ds) p
  return (Layer p)
  where
    selectExc GDALException{..} | gdalErrNum==AppDefined = Just gdalErrMsg
    selectExc _                                          = Nothing
    execute =
      throwIfError "executeSQL" $
      liftIO $
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      useAsEncodedCString query $ \sQuery ->
      withSQLDialect dialect $
        {#call OGR_DS_ExecuteSQL as ^#} (unDataSource ds) sQuery sFilter

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
