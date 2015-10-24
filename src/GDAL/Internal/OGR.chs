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

  , withLockedLayerPtr
  , withLockedLayerPtrs
  , unDataSource
  , unLayer
  , withLockedDataSourcePtr
  , layerHasCapability
  , driverHasCapability
  , dataSourceHasCapability
) where

{#context lib = "gdal" prefix = "OGR"#}

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
newtype DataSource s (t::AccessMode) = DataSource (Mutex, DataSourceH)

deriving instance Eq DataSourceH

nullDataSourceH :: DataSourceH
nullDataSourceH = DataSourceH nullPtr

unDataSource :: DataSource s t -> DataSourceH
unDataSource (DataSource (_,p)) = p

type RODataSource s = DataSource s ReadOnly
type RWDataSource s = DataSource s ReadWrite

withLockedDataSourcePtr
  :: DataSource s t -> (DataSourceH -> IO b) -> IO b
withLockedDataSourcePtr (DataSource (m,p)) f = withMutex m (f p)



{#pointer LayerH newtype#}

deriving instance Storable LayerH
deriving instance Eq LayerH

nullLayerH :: LayerH
nullLayerH = LayerH nullPtr

newtype (Layer s (t::AccessMode) a) = Layer (Mutex, LayerH)


unLayer :: Layer s t a -> LayerH
unLayer (Layer (_,p)) = p

lMutex :: Layer s t a -> Mutex
lMutex (Layer (m,_)) = m

withLockedLayerPtr
  :: Layer s t a -> (LayerH -> IO b) -> IO b
withLockedLayerPtr (Layer (m,p)) f = withMutex m $ f p

withLockedLayerPtrs
  :: [Layer s t a] -> ([LayerH] -> IO b) -> IO b
withLockedLayerPtrs ls f
  = withMutexes (map lMutex ls) (f (map unLayer ls))

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
           throwIfError "open" ({#call OGROpen as ^#} p' (fromEnumC m) nullPtr)
  newDataSourceHandle ptr `catch` (\NullDataSource ->
    throwM (GDALException CE_Failure OpenFailed "OGROpen returned a NULL ptr"))


newDataSourceHandle :: DataSourceH -> GDAL s (DataSource s t)
newDataSourceHandle p
  | p==nullDataSourceH = throwBindingException NullDataSource
  | otherwise          = do
      registerFinalizer (void ({#call unsafe ReleaseDataSource as ^#} p))
      m <- liftIO newMutex
      return $ DataSource (m,p)

type Driver = String

create :: Driver -> String -> OptionList -> GDAL s (RWDataSource s)
create driverName name options = newDataSourceHandle <=<
  liftIO $ throwIfError "create" $ do
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
createLayerWithDef ds FeatureDef{..} approxOk options = liftIO $
  useAsEncodedCString fdName $ \pName ->
  withMaybeSpatialReference (gfdSrs fdGeom) $ \pSrs ->
  withOptionList options $ \pOpts ->
  throwIfError "createLayer" $ do
    fpL <- {#call OGR_DS_CreateLayer as ^#} pDs pName pSrs gType pOpts >>=
              newLayerHandle ds NullLayer
    withLockedLayerPtr fpL $ \pL -> do
      V.forM_ fdFields $ \(n,f) -> withFieldDefnH n f $ \pFld ->
        {#call unsafe OGR_L_CreateField as ^#} pL pFld iApproxOk
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
      return fpL
  where
    supportsMultiGeomFields pL =
      layerHasCapability pL CreateGeomField &&
      dataSourceHasCapability pDs CreateGeomFieldAfterCreateLayer
    iApproxOk = fromEnumC approxOk
    pDs   = unDataSource ds
    gType = fromEnumC (gfdType fdGeom)


createFeature :: OGRFeature a => RWLayer s a -> a -> GDAL s Fid
createFeature layer feature = liftIO $ throwIfError "createFeature" $
  withLockedLayerPtr layer $ \pL -> do
    pFd <- {#call unsafe OGR_L_GetLayerDefn as ^#} pL
    featureToHandle pFd Nothing feature $ \pF -> do
      void $ {#call OGR_L_CreateFeature as ^#} pL pF
      getFid pF >>= maybe (throwBindingException UnexpectedNullFid) return

createFeature_ :: OGRFeature a => RWLayer s a -> a -> GDAL s ()
createFeature_ layer feature =
  catchJust (\case {UnexpectedNullFid -> Just (); _ -> Nothing})
            (void (createFeature layer feature))
            return


setFeature :: OGRFeature a => RWLayer s a -> Fid -> a -> GDAL s ()
setFeature layer fid feature = liftIO $ throwIfError "setFeature" $
  withLockedLayerPtr layer $ \pL -> do
    pFd <- {#call unsafe OGR_L_GetLayerDefn as ^#} pL
    void $ featureToHandle pFd (Just fid) feature
      ({#call OGR_L_SetFeature as ^#} pL)

getFeature :: OGRFeature a => Layer s t a -> Fid -> GDAL s (Maybe a)
getFeature layer (Fid fid) = liftIO $ do
  withLockedLayerPtr layer $ \pL -> do
    when (not (pL `layerHasCapability` RandomRead)) $
      throwBindingException (UnsupportedLayerCapability RandomRead)
    pFd <- {#call unsafe OGR_L_GetLayerDefn as ^#} pL
    liftM (fmap snd) $ featureFromHandle pFd $
      {#call OGR_L_GetFeature as ^#} pL (fromIntegral fid)

deleteFeature :: Layer s t a -> Fid -> GDAL s ()
deleteFeature layer (Fid fid) = liftIO $
  throwIfError "deleteFeature" $
    void $ withLockedLayerPtr layer $
      flip {#call OGR_L_DeleteFeature as ^#} (fromIntegral fid)

canCreateMultipleGeometryFields :: Bool
canCreateMultipleGeometryFields =
#if SUPPORTS_MULTI_GEOM_FIELDS
  True
#else
  False
#endif

getLayer :: Int -> DataSource s t -> GDAL s (Layer s t a)
getLayer layer ds = liftIO $
  newLayerHandle ds (InvalidLayerIndex layer) <=<
    throwIfError "getLayer" $ {#call OGR_DS_GetLayer as ^#} dsH lyr
  where
    dsH = unDataSource ds
    lyr = fromIntegral layer

getLayerByName :: Text -> DataSource s t -> GDAL s (Layer s t a)
getLayerByName layer ds = liftIO $ useAsEncodedCString layer $
  newLayerHandle ds (InvalidLayerName layer) <=<
    throwIfError "getLayerByName" .
      {#call OGR_DS_GetLayerByName as ^#} (unDataSource ds)

-- | GDAL < 1.11 only supports 1 geometry field and associates it the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> ({#call unsafe OGR_L_GetSpatialRef as ^#} p >>=
          maybeNewSpatialRefBorrowedHandle)
    <*> pure True

newLayerHandle
  :: DataSource s t -> OGRException -> LayerH -> IO (Layer s t a)
newLayerHandle (DataSource (m,_)) exc p
  | p==nullLayerH = throwBindingException exc
  | otherwise     = return (Layer (m,p))

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
executeSQL dialect query mSpatialFilter ds@(DataSource (m,dsP)) = do
  p <- catchJust selectExc execute (throwBindingException . SQLQueryError)
  when (p==nullLayerH) $ throwBindingException NullLayer
  registerFinalizer ({#call unsafe OGR_DS_ReleaseResultSet as ^#} dsP p)
  return (Layer (m, p))
  where
    selectExc GDALException{..} | gdalErrNum==AppDefined = Just gdalErrMsg
    selectExc _                                          = Nothing
    execute = liftIO $
      throwIfError "executeSQL" $
      withLockedDataSourcePtr ds $ \dsPtr ->
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      withSQLDialect dialect $ \sDialect ->
      useAsEncodedCString query $ \sQuery ->
        {#call OGR_DS_ExecuteSQL as ^#} dsPtr sQuery sFilter sDialect

layerName :: Layer s t a -> GDAL s Text
layerName =
  liftIO . (peekEncodedCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

layerFeatureDef :: Layer s t a -> GDAL s FeatureDef
layerFeatureDef l = liftIO $ do
  let pL = unLayer l
  gfd <- layerGeomFieldDef pL
  featureDefFromHandle gfd =<< {#call unsafe OGR_L_GetLayerDefn as ^#} pL


getSpatialFilter :: Layer s t a -> GDAL s (Maybe Geometry)
getSpatialFilter l = liftIO $ withLockedLayerPtr l $ \lPtr -> do
  p <- {#call unsafe OGR_L_GetSpatialFilter as ^#} lPtr
  if p == nullPtr
    then return Nothing
    else liftM Just (cloneGeometry p)

setSpatialFilter :: Layer s t a -> Geometry -> GDAL s ()
setSpatialFilter l g = liftIO $
  withLockedLayerPtr l $ \lPtr -> withGeometry g$ \gPtr ->
    {#call unsafe OGR_L_SetSpatialFilter as ^#} lPtr gPtr

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
