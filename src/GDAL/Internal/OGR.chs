{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.OGR (
    GeometryType (..)
  , Geometry
  , ROGeometry
  , RWGeometry
  , Datasource
  , SQLDialect (..)
  , WkbByteOrder (..)
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

  , createFromWktIO
  , createFromWkbIO
  , exportToWktIO
  , exportToWkbIO

  , createFromWkt
  , createFromWkb
  , exportToWkt
  , exportToWkb

  , unsafeThawGeometry
  , unsafeFreezeGeometry

  , datasourceName
  , executeSQL

  , getLayer
  , getLayerByName

  , getSpatialFilterRef
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
import Data.ByteString.Char8 (packCString, unpack)
import Data.ByteString.Unsafe (
    unsafeUseAsCString
  , unsafeUseAsCStringLen
  , unsafePackMallocCStringLen
  )
import Data.Coerce (coerce)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..), CUChar(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr)
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
import GDAL.Internal.OGRError
import GDAL.Internal.OSR
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.Util

#include "ogr_api.h"

{#context prefix = "OGR" #}

{#fun OGRRegisterAll as registerAllDrivers {} -> `()'  #}
{#fun OGRCleanupAll  as cleanupAll {} -> `()'  #}


{#pointer OGRDataSourceH as Datasource foreign newtype nocode#}

newtype Datasource s (t::AccessMode) a
  = Datasource (Mutex, Ptr (Datasource s t a))

unDatasource :: Datasource s t a -> Ptr (Datasource s t a)
unDatasource (Datasource (_,p)) = p

type RODatasource s = Datasource s ReadOnly
type RWDatasource s = Datasource s ReadWrite

withLockedDatasourcePtr
  :: Datasource s t a -> (Ptr (Datasource s t a) -> IO b) -> IO b
withLockedDatasourcePtr (Datasource (m,p)) f = withMutex m (f p)


{#enum define OGRAccess
  { FALSE  as OGR_ReadOnly
  , TRUE   as OGR_Update
  } deriving (Eq, Show) #}

{#enum define WkbByteOrder
  { wkbXDR  as WkbXDR
  , wkbNDR  as WkbNDR
  } deriving (Eq, Show) #}

openReadOnly :: String -> GDAL s (RODatasource s a)
openReadOnly p = openWithMode OGR_ReadOnly p

openReadWrite :: String -> GDAL s (RWDatasource s a)
openReadWrite p = openWithMode OGR_Update p

openWithMode :: OGRAccess -> String -> GDAL s (Datasource s t a)
openWithMode m p = do
  ptr <- liftIO $ withCString p $ \p' ->
           throwIfError "open" (c_open p' (fromEnumC m) nullPtr)
  newDatasourceHandle ptr `catch` (\NullDatasource ->
    throwM (GDALException CE_Failure OpenFailed "OGROpen returned a NULL ptr"))

foreign import ccall safe "ogr_api.h OGROpen" c_open
   :: CString -> CInt -> Ptr () -> IO (Ptr (Datasource s t a))

newDatasourceHandle :: Ptr (Datasource s t a) -> GDAL s (Datasource s t a)
newDatasourceHandle p
  | p==nullPtr  = throwBindingException NullDatasource
  | otherwise   = do
      registerFinalizer (c_releaseDatasource p)
      m <- liftIO newMutex
      return $ Datasource (m,p)

foreign import ccall safe "ogr_api.h OGRReleaseDataSource"
  c_releaseDatasource :: Ptr (Datasource s t a) -> IO ()


getLayer :: Int -> Datasource s t a -> GDAL s (Layer s t a)
getLayer layer (Datasource (m,dp)) = liftIO $ do
  p <- throwIfError "getLayer" (c_getLayer dp (fromIntegral layer))
  when (p==nullPtr) $ throwBindingException (InvalidLayerIndex layer)
  return (Layer (m, p))

foreign import ccall safe "ogr_api.h OGR_DS_GetLayer" c_getLayer
  :: Ptr (Datasource s t a) -> CInt -> IO (Ptr (Layer s t a))

getLayerByName :: String -> Datasource s t a -> GDAL s (Layer s t a)
getLayerByName layer (Datasource (m,dp)) = liftIO $
  withCString layer $ \lyr -> do
    p <- throwIfError "getLayerByName" (c_getLayerByName dp lyr)
    when (p==nullPtr) $ throwBindingException (InvalidLayerName layer)
    return (Layer (m,p))

foreign import ccall safe "ogr_api.h OGR_DS_GetLayerByName" c_getLayerByName
  :: Ptr (Datasource s t a) -> CString -> IO (Ptr (Layer s t a))


layerCount :: Datasource s t a -> GDAL s Int
layerCount = liftM fromIntegral . liftIO . c_getLayerCount . unDatasource

foreign import ccall unsafe "ogr_api.h OGR_DS_GetLayerCount" c_getLayerCount
  :: Ptr (Datasource s t a) -> IO CInt

datasourceName :: Datasource s t a -> GDAL s String
datasourceName = liftIO . (peekCString <=< c_dsGeName . unDatasource)

foreign import ccall unsafe "ogr_api.h OGR_DS_GetName" c_dsGeName
  :: Ptr (Datasource s t a) -> IO CString

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
  :: SQLDialect -> String -> Maybe ROGeometry -> RODatasource s a
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
    :: Ptr (RODatasource s a) -> CString -> Ptr ROGeometry -> CString
    -> IO (Ptr (ROLayer s a))

foreign import ccall safe "ogr_api.h OGR_DS_ReleaseResultSet"
  c_releaseResultSet
    :: Ptr (RODatasource s a) -> Ptr (ROLayer s a) -> IO ()






{#pointer OGRLayerH as Layer newtype nocode#}

newtype (Layer s (t::AccessMode) a)
  = Layer (Mutex, Ptr (Layer s t a))

unLayer :: Layer s t a -> Ptr (Layer s t a)
unLayer (Layer (_,p)) = p

withLockedLayerPtr
  :: Layer s t a -> (Ptr (Layer s t a) -> IO b) -> IO b
withLockedLayerPtr (Layer (m,p)) f = withMutex m $ f p

type ROLayer s = Layer s ReadOnly
type RWLayer s = Layer s ReadWrite

layerName :: Layer s t a -> GDAL s String
layerName = liftIO . (peekCString <=< c_lGetName . unLayer)

foreign import ccall unsafe "ogr_api.h OGR_L_GetName" c_lGetName
  :: Ptr (Layer s t a) -> IO CString


getSpatialFilter :: Layer s t a -> GDAL s (Maybe ROGeometry)
getSpatialFilter
  = maybe (return Nothing) (liftM Just . deRef) <=< getSpatialFilterRef

getSpatialFilterRef :: Layer s t a -> GDAL s (Maybe (Ref s Geometry ReadOnly))
getSpatialFilterRef l = liftIO $ withLockedLayerPtr l $ \lPtr -> do
  p <- c_getSpatialFilter lPtr
  if p == nullPtr
    then return Nothing
    else liftM (Just . mkRef . Geometry) (newForeignPtr_ p)

foreign import ccall unsafe "ogr_api.h OGR_L_GetSpatialFilter"
  c_getSpatialFilter :: Ptr (Layer s t a) -> IO (Ptr ROGeometry)

setSpatialFilter :: Layer s t a -> Geometry t' -> GDAL s ()
setSpatialFilter l g = liftIO $
  withLockedLayerPtr l $ \lPtr -> withGeometry g$ \gPtr ->
    c_setSpatialFilter lPtr gPtr

foreign import ccall unsafe "ogr_api.h OGR_L_SetSpatialFilter"
  c_setSpatialFilter :: Ptr (Layer s t a) -> Ptr (Geometry t') -> IO ()

newtype Geometry (t::AccessMode) = Geometry (ForeignPtr (Geometry t))

instance Clonable Geometry where
  clone = liftIO . flip withGeometry (c_cloneGeometry >=> newGeometryHandle)

foreign import ccall safe "ogr_api.h OGR_G_Clone"
  c_cloneGeometry :: Ptr (Geometry t) -> IO (Ptr (Geometry t'))


withMaybeGeometry :: Maybe (Geometry t) -> (Ptr (Geometry t) -> IO a) -> IO a
withMaybeGeometry (Just (Geometry ptr)) = withForeignPtr ptr
withMaybeGeometry Nothing               = ($ nullPtr)

withGeometry :: Geometry t -> (Ptr (Geometry t) -> IO a) -> IO a
withGeometry (Geometry ptr) = withForeignPtr ptr

type ROGeometry = Geometry ReadOnly
type RWGeometry = Geometry ReadWrite

foreign import ccall "ogr_api.h &OGR_G_DestroyGeometry"
  c_destroyGeometry :: FunPtr (Ptr (Geometry t) -> IO ())

newGeometryHandle :: Ptr (Geometry t) -> IO (Geometry t)
newGeometryHandle p
  | p==nullPtr = throwBindingException NullGeometry
  | otherwise  = Geometry <$> newForeignPtr c_destroyGeometry p

createFromWkb
  :: Maybe SpatialReference -> ByteString -> Either OGRError ROGeometry
createFromWkb mSr = unsafePerformIO . createFromWkbIO mSr

createFromWkbIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError (Geometry t))
createFromWkbIO mSrs bs =
  alloca $ \gPtr ->
  unsafeUseAsCStringLen bs $ \(sPtr, len) ->
  withMaybeSpatialReference mSrs $ \srs ->
    checkOGRError
      (c_createFromWkb sPtr srs gPtr (fromIntegral len))
      (peek gPtr >>= newGeometryHandle)

foreign import ccall unsafe "ogr_api.h OGR_G_CreateFromWkb"
  c_createFromWkb ::
    CString -> Ptr SpatialReference -> Ptr (Ptr (Geometry t)) -> CInt -> IO CInt


createFromWkt
  :: Maybe SpatialReference -> ByteString -> Either OGRError ROGeometry
createFromWkt mSrs = unsafePerformIO . createFromWktIO mSrs

createFromWktIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError (Geometry t))
createFromWktIO mSrs bs =
  alloca $ \gPtr ->
  alloca $ \sPtrPtr ->
  unsafeUseAsCString bs $ \sPtr ->
  withMaybeSpatialReference mSrs $ \srs ->
    checkOGRError
      (poke sPtrPtr sPtr >> c_createFromWkt sPtrPtr srs gPtr)
      (peek gPtr >>= newGeometryHandle)

foreign import ccall unsafe "ogr_api.h OGR_G_CreateFromWkt"
  c_createFromWkt ::
    Ptr CString -> Ptr SpatialReference -> Ptr (Ptr (Geometry t)) -> IO CInt

unsafeFreezeGeometry :: RWGeometry -> ROGeometry
unsafeFreezeGeometry = coerce

unsafeThawGeometry :: RWGeometry -> ROGeometry
unsafeThawGeometry = coerce

withMaybeSpatialReference
  :: Maybe SpatialReference -> (Ptr SpatialReference -> IO a) -> IO a
withMaybeSpatialReference Nothing  = ($ nullPtr)
withMaybeSpatialReference (Just s) = withSpatialReference s

peekAndPack :: Ptr CString -> IO ByteString
peekAndPack = peek >=> packCString

exportToWktIO :: Geometry t -> IO ByteString
exportToWktIO g = withGeometry g $ \gPtr -> alloca $ \sPtrPtr -> do
  void $ {#call OGR_G_ExportToWkt as ^ #} (castPtr gPtr) sPtrPtr
  peekAndPack sPtrPtr

exportToWkt :: ROGeometry -> ByteString
exportToWkt = unsafePerformIO . exportToWktIO

exportToWkbIO :: WkbByteOrder -> Geometry t -> IO ByteString
exportToWkbIO bo g = withGeometry g $ \gPtr -> do
  len <- liftM fromIntegral ({#call OGR_G_WkbSize as ^ #} (castPtr gPtr))
  buf <- mallocBytes len
  void $ {#call OGR_G_ExportToWkb as ^ #} (castPtr gPtr) (fromEnumC bo) buf
  unsafePackMallocCStringLen (castPtr buf, len)

exportToWkb :: WkbByteOrder -> ROGeometry -> ByteString
exportToWkb bo = unsafePerformIO . exportToWkbIO bo

geomEqIO :: Geometry t -> Geometry t1 -> IO Bool
geomEqIO a b = withGeometry a $ \aPtr -> withGeometry b $ \bPtr ->
  liftM toBool ({#call OGR_G_Equals as ^#} (castPtr aPtr) (castPtr bPtr))

instance Show ROGeometry where
  show = unpack . exportToWkt

instance Eq ROGeometry where
  a == b = unsafePerformIO (geomEqIO a b)

{#enum OGRwkbGeometryType as GeometryType {underscoreToCase}#}

instance Show GeometryType where
  show s = unsafePerformIO $
            {#call unsafe OGRGeometryTypeToName as ^#} s' >>= peekCString
    where s' = fromEnumC s
