{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.OGR (
    Datasource
  , SQLDialect (..)
  , Layer
  , RODatasource
  , RWDatasource
  , ROLayer
  , RWLayer

  , unDatasource
  , unLayer
  , withLockedDatasourcePtr
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

import Control.Applicative ((<$>))
import Control.Monad (liftM, when, void, (<=<), (>=>))
import Control.Monad.Catch(throwM, catch, catchJust)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Unsafe (
    unsafeUseAsCString
  , unsafeUseAsCStringLen
  , unsafePackMallocCStringLen
  , unsafePackCStringFinalizer
  )
import Data.Coerce (coerce)
import Data.Word (Word8)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..), CUChar(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr, plusPtr)
import Foreign.ForeignPtr (
    ForeignPtr
  , withForeignPtr
  , newForeignPtr
  , newForeignPtr_
  )
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek, poke)

import System.IO.Unsafe (unsafePerformIO)

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


{#pointer OGRDataSourceH as Datasource foreign newtype nocode#}

newtype Datasource s (t::AccessMode)
  = Datasource (Mutex, Ptr (Datasource s t))

unDatasource :: Datasource s t -> Ptr (Datasource s t)
unDatasource (Datasource (_,p)) = p

type RODatasource s = Datasource s ReadOnly
type RWDatasource s = Datasource s ReadWrite

withLockedDatasourcePtr
  :: Datasource s t -> (Ptr (Datasource s t) -> IO b) -> IO b
withLockedDatasourcePtr (Datasource (m,p)) f = withMutex m (f p)


{#enum define OGRAccess
  { FALSE  as OGR_ReadOnly
  , TRUE   as OGR_Update
  } deriving (Eq, Show) #}


openReadOnly :: String -> GDAL s (RODatasource s)
openReadOnly p = openWithMode OGR_ReadOnly p

openReadWrite :: String -> GDAL s (RWDatasource s)
openReadWrite p = openWithMode OGR_Update p

openWithMode :: OGRAccess -> String -> GDAL s (Datasource s t)
openWithMode m p = do
  ptr <- liftIO $ withCString p $ \p' ->
           throwIfError "open" (c_open p' (fromEnumC m) nullPtr)
  newDatasourceHandle ptr `catch` (\NullDatasource ->
    throwM (GDALException CE_Failure OpenFailed "OGROpen returned a NULL ptr"))

foreign import ccall safe "ogr_api.h OGROpen" c_open
   :: CString -> CInt -> Ptr () -> IO (Ptr (Datasource s t))

newDatasourceHandle :: Ptr (Datasource s t) -> GDAL s (Datasource s t)
newDatasourceHandle p
  | p==nullPtr  = throwBindingException NullDatasource
  | otherwise   = do
      registerFinalizer (c_releaseDatasource p)
      m <- liftIO newMutex
      return $ Datasource (m,p)

foreign import ccall safe "ogr_api.h OGRReleaseDataSource"
  c_releaseDatasource :: Ptr (Datasource s t) -> IO ()


getLayer :: Int -> Datasource s t -> GDAL s (Layer s t a)
getLayer layer (Datasource (m,dp)) = liftIO $ do
  p <- throwIfError "getLayer" (c_getLayer dp (fromIntegral layer))
  when (p==nullPtr) $ throwBindingException (InvalidLayerIndex layer)
  return (Layer (m, p))

foreign import ccall safe "ogr_api.h OGR_DS_GetLayer" c_getLayer
  :: Ptr (Datasource s t) -> CInt -> IO (Ptr (Layer s t a))

getLayerByName :: String -> Datasource s t -> GDAL s (Layer s t a)
getLayerByName layer (Datasource (m,dp)) = liftIO $
  withCString layer $ \lyr -> do
    p <- throwIfError "getLayerByName" (c_getLayerByName dp lyr)
    when (p==nullPtr) $ throwBindingException (InvalidLayerName layer)
    return (Layer (m,p))

foreign import ccall safe "ogr_api.h OGR_DS_GetLayerByName" c_getLayerByName
  :: Ptr (Datasource s t) -> CString -> IO (Ptr (Layer s t a))


layerCount :: Datasource s t -> GDAL s Int
layerCount = liftM fromIntegral . liftIO . c_getLayerCount . unDatasource

foreign import ccall unsafe "ogr_api.h OGR_DS_GetLayerCount" c_getLayerCount
  :: Ptr (Datasource s t) -> IO CInt

datasourceName :: Datasource s t -> GDAL s String
datasourceName = liftIO . (peekCString <=< c_dsGeName . unDatasource)

foreign import ccall unsafe "ogr_api.h OGR_DS_GetName" c_dsGeName
  :: Ptr (Datasource s t) -> IO CString

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
  :: SQLDialect -> String -> Maybe Geometry -> RODatasource s
  -> GDAL s (ROLayer s a)
executeSQL dialect query mSpatialFilter ds@(Datasource (m,dsP)) = do
  p <- catchJust selectExc execute (throwBindingException . SQLQueryError)
  when (p==nullPtr) $ throwBindingException NullLayer
  registerFinalizer (c_releaseResultSet dsP p)
  return (Layer (m, p))
  where
    selectExc GDALException{..} | gdalErrNum==AppDefined = Just gdalErrMsg
    selectExc _                                          = Nothing
    execute = liftIO $
      withLockedDatasourcePtr ds $ \dsPtr ->
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      withSQLDialect dialect $ \sDialect ->
      withCString query $ \sQuery ->
        throwIfError "executeSQL" (c_executeSQL dsPtr sQuery sFilter sDialect)

foreign import ccall safe "ogr_api.h OGR_DS_ExecuteSQL"
  c_executeSQL
    :: Ptr (RODatasource s) -> CString -> Ptr Geometry -> CString
    -> IO (Ptr (ROLayer s a))

foreign import ccall safe "ogr_api.h OGR_DS_ReleaseResultSet"
  c_releaseResultSet
    :: Ptr (RODatasource s) -> Ptr (ROLayer s a) -> IO ()






{#pointer OGRLayerH as Layer newtype nocode#}

newtype (Layer s (t::AccessMode) a)
  = Layer (Mutex, Ptr (Layer s t a))

unLayer :: Layer s t a -> Ptr (Layer s t a)
unLayer (Layer (_,p)) = p

lMutex :: Layer s t a -> Mutex
lMutex (Layer (m,_)) = m

withLockedLayerPtr
  :: Layer s t a -> (Ptr (Layer s t a) -> IO b) -> IO b
withLockedLayerPtr (Layer (m,p)) f = withMutex m $ f p

withLockedLayerPtrs
  :: [Layer s t a] -> ([Ptr (Layer s t a)] -> IO b) -> IO b
withLockedLayerPtrs ls f
  = withMutexes (map lMutex ls) (f (map unLayer ls))

type ROLayer s = Layer s ReadOnly
type RWLayer s = Layer s ReadWrite

layerName :: Layer s t a -> GDAL s String
layerName = liftIO . (peekCString <=< c_lGetName . unLayer)

foreign import ccall unsafe "ogr_api.h OGR_L_GetName" c_lGetName
  :: Ptr (Layer s t a) -> IO CString


getSpatialFilter :: Layer s t a -> GDAL s (Maybe Geometry)
getSpatialFilter l = liftIO $ withLockedLayerPtr l $ \lPtr -> do
  p <- c_getSpatialFilter lPtr
  if p == nullPtr
    then return Nothing
    else liftM Just (cloneGeometry p)

foreign import ccall unsafe "ogr_api.h OGR_L_GetSpatialFilter"
  c_getSpatialFilter :: Ptr (Layer s t a) -> IO (Ptr Geometry)

setSpatialFilter :: Layer s t a -> Geometry -> GDAL s ()
setSpatialFilter l g = liftIO $
  withLockedLayerPtr l $ \lPtr -> withGeometry g$ \gPtr ->
    c_setSpatialFilter lPtr gPtr

foreign import ccall unsafe "ogr_api.h OGR_L_SetSpatialFilter"
  c_setSpatialFilter :: Ptr (Layer s t a) -> Ptr Geometry -> IO ()
