{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

  , unDataSource
  , unLayer
  , withLockedDataSourcePtr
  , openReadOnly
  , openReadWrite
  , withLockedLayerPtr
  , withLockedLayerPtrs


  , datasourceName
  , executeSQL

  , getLayer
  , getLayerByName

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
  , layerName

  , registerAllDrivers
  , cleanupAll
) where

{#context lib = "gdal" prefix = "OGR"#}

import Control.Applicative ((<$>))
import Control.Monad (liftM, when, void, (<=<))
import Control.Monad.Catch(throwM, catch, catchJust)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, nullPtr)

import Foreign.Storable (Storable)

import GDAL.Internal.Types
{#import GDAL.Internal.OGRGeometry#}
{#import GDAL.Internal.OGRFeature#}
{#import GDAL.Internal.OGRError#}
import GDAL.Internal.OSR
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLConv (cplFree)
import GDAL.Internal.Util

#include "ogr_api.h"

{#fun OGRRegisterAll as registerAllDrivers {} -> `()'  #}
{#fun OGRCleanupAll  as cleanupAll {} -> `()'  #}


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


getLayer :: Int -> DataSource s t -> GDAL s (Layer s t a)
getLayer layer (DataSource (m,dp)) = liftIO $ do
  p <- throwIfError "getLayer" $
         {#call OGR_DS_GetLayer as ^#} dp (fromIntegral layer)
  when (p==nullLayerH) $ throwBindingException (InvalidLayerIndex layer)
  return (Layer (m, p))

getLayerByName :: String -> DataSource s t -> GDAL s (Layer s t a)
getLayerByName layer (DataSource (m,dp)) = liftIO $
  withCString layer $ \lyr -> do
    p <- throwIfError "getLayerByName" $
          {#call OGR_DS_GetLayerByName as ^#} dp lyr
    when (p==nullLayerH) $ throwBindingException (InvalidLayerName layer)
    return (Layer (m,p))


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
  :: SQLDialect -> String -> Maybe Geometry -> RODataSource s
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
      withLockedDataSourcePtr ds $ \dsPtr ->
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      withSQLDialect dialect $ \sDialect ->
      withCString query $ \sQuery ->
      throwIfError "executeSQL" $
        {#call OGR_DS_ExecuteSQL as ^#} dsPtr sQuery sFilter sDialect

layerName :: Layer s t a -> GDAL s String
layerName =
  liftIO . (peekCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

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
