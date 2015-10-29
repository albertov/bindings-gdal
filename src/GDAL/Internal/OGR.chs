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
  , Envelope (..)
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
  , executeSQL_

  , syncToDisk

  , getSpatialFilter
  , setSpatialFilter

  , layerCount
  , layerName
  , layerExtent
  , layerFeatureDef

  , createFeature
  , createFeatureWithFid
  , createFeature_
  , getFeature
  , updateFeature
  , deleteFeature

  , registerAll
  , cleanupAll

  , unDataSource
  , unLayer
  , layerHasCapability
  , driverHasCapability
  , dataSourceHasCapability
  , nullLayerH
) where

{#context lib = "gdal" prefix = "OGR"#}

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Vector as V

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, when, void, (>=>), (<=<))
import Control.Monad.Catch(catchJust, throwM, bracketOnError, bracket, finally)
import Control.Monad.Trans(lift)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..), CLong(..))
#if GDAL_VERSION_MAJOR >= 2
import Foreign.C.Types (CLLong(..))
#endif
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

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
openWithMode m p =
  catchJust
    (\case {GDALException{gdalErrNum=AssertionFailed}->Just ();_->Nothing})
    (newDataSourceHandle $ withCString p $ \p' ->
      {#call OGROpen as ^#} p' (fromEnumC m) nullPtr)
    (const (throwM (GDALException CE_Failure OpenFailed "open: failed")))

newDataSourceHandle
  :: IO DataSourceH -> GDAL s (DataSource s t)
newDataSourceHandle act = liftM snd $ allocate alloc free
  where
    alloc = liftM DataSource (checkReturns (/=nullDataSourceH) act)
    free  = {#call OGR_DS_Destroy as ^#} . unDataSource

type Driver = String

create :: Driver -> String -> OptionList -> GDAL s (RWDataSource s)
create driverName name options = newDataSourceHandle $ do
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
  --throwIfError "createLayerWithDef" $
  liftIO $
  useAsEncodedCString fdName $ \pName ->
  withMaybeSpatialReference (gfdSrs fdGeom) $ \pSrs ->
  withOptionList options $ \pOpts -> do
    pL <- {#call OGR_DS_CreateLayer as ^#} pDs pName pSrs gType pOpts
    when (pL == nullLayerH) (throwBindingException NullLayer)
    V.forM_ fdFields $ \(n,f) -> withFieldDefnH n f $ \pFld ->
      checkOGRError $ {#call unsafe OGR_L_CreateField as ^#} pL pFld iApproxOk
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
    checkOGRError ({#call OGR_L_CreateFeature as ^#} pL pF)
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

updateFeature :: OGRFeature a => RWLayer s a -> Fid -> a -> GDAL s ()
updateFeature layer fid feat = liftIO $ do
  pFd <- getLayerSchema pL
  featureToHandle pFd (Just fid) feat $
    checkOGRError . {#call OGR_L_SetFeature as ^#} pL
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
  checkOGRError $
    {#call OGR_L_DeleteFeature as ^#} (unLayer layer) (fromIntegral fid)

canCreateMultipleGeometryFields :: Bool
canCreateMultipleGeometryFields =
#if SUPPORTS_MULTI_GEOM_FIELDS
  True
#else
  False
#endif

syncToDiskIO :: RWLayer s a -> IO ()
syncToDiskIO = checkOGRError . {#call OGR_L_SyncToDisk as ^#} . unLayer

syncToDisk :: RWLayer s a -> GDAL s ()
syncToDisk = liftIO . syncToDiskIO


getLayer :: Int -> DataSource s t -> GDAL s (Layer s t a)
getLayer ix ds = liftIO $ do
  pL <- {#call OGR_DS_GetLayer as ^#} (unDataSource ds) (fromIntegral ix)
  when (pL == nullLayerH) (throwBindingException (InvalidLayerIndex ix))
  return (Layer pL)


getLayerByName :: Text -> DataSource s t -> GDAL s (Layer s t a)
getLayerByName name ds = liftIO $ useAsEncodedCString name $ \lName -> do
  pL <- {#call OGR_DS_GetLayerByName as ^#} (unDataSource ds) lName
  when (pL==nullLayerH) (throwBindingException (InvalidLayerName name))
  return (Layer pL)


sourceLayer
  :: OGRFeature a => GDAL s (Layer s t a) -> GDALSource s (Maybe Fid, a)
sourceLayer = flip sourceFromLayer (const (return ()))

sourceLayer_
  :: OGRFeature a => GDAL s (Layer s t a) -> GDALSource s a
sourceLayer_ = (=$= (CL.map snd)) . sourceLayer


conduitInsertLayer
  :: OGRFeature a
  => GDAL s (RWLayer s a) -> GDALConduit s (Maybe Fid, a) (Maybe Fid)
conduitInsertLayer = flip conduitFromLayer createIt
  where
    createIt (l, pFd) = awaitForever $ \(fid, feat) -> do
      fid' <- liftIO $ featureToHandle pFd fid feat $ \pF -> do
                checkOGRError ({#call OGR_L_CreateFeature as ^#} (unLayer l) pF)
                getFid pF
      yield fid'

conduitInsertLayer_
  :: OGRFeature a
  => GDAL s (RWLayer s a) -> GDALConduit s a (Maybe Fid)
conduitInsertLayer_ = (CL.map (\i->(Nothing,i)) =$=) . conduitInsertLayer

sinkInsertLayer
  :: OGRFeature a => GDAL s (RWLayer s a) -> GDALSink s (Maybe Fid, a) ()
sinkInsertLayer = (=$= CL.sinkNull) . conduitInsertLayer

sinkInsertLayer_
  :: OGRFeature a => GDAL s (RWLayer s a) -> GDALSink s a ()
sinkInsertLayer_ =
  ((=$= CL.sinkNull) . ((CL.map (\i->(Nothing,i))) =$=)) . conduitInsertLayer

sinkUpdateLayer
  :: OGRFeature a => GDAL s (RWLayer s a) -> GDALSink s (Fid, a) ()
sinkUpdateLayer = flip conduitFromLayer updateIt
  where
    updateIt (l, pFd) = awaitForever $ \(fid, feat) ->
      liftIO $ featureToHandle pFd (Just fid) feat $
        checkOGRError . {#call OGR_L_SetFeature as ^#} (unLayer l)


layerTransaction
  :: GDAL s (Layer s t a, e)
  -> ((Layer s t a, e) -> IO ())
  -> ((Layer s t a, e) -> GDALConduit s i o)
  -> GDALConduit s i o
layerTransaction alloc free inside = do
  alloc' <- lift (unsafeGDALToIO alloc)
  (rbKey, seed) <- allocate alloc' rollback
  liftIO $ checkOGRError $
    {#call OGR_L_StartTransaction as ^#} (unLayer (fst seed))
  addCleanup (const (release rbKey))
             (inside seed >> liftIO (commit rbKey seed))
  where
    commit rbKey seed = bracket (unprotect rbKey) (const (free seed)) $ \m -> do
      when (isNothing m) (error "layerTransaction: this should not happen")
      checkOGRError ({#call OGR_L_CommitTransaction as ^#} (unLayer (fst seed)))

    rollback seed =
      checkOGRError ({#call OGR_L_RollbackTransaction as ^#} (unLayer (fst seed)))
        `finally` free seed

sourceFromLayer
  :: forall s t a. OGRFeature a
  => GDAL s (Layer s t a)
  -> (Layer s t a -> IO ())
  -> GDALSource s (Maybe Fid, a)
sourceFromLayer alloc free = layerTransaction alloc' (free . fst) loop
  where
    alloc' = bracketOnError alloc (liftIO . free) $ \l -> do
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


conduitFromLayer
  :: GDAL s (RWLayer s a) -> ((RWLayer s a, FeatureDefnH) -> GDALConduit s i o)
  -> GDALConduit s i o
conduitFromLayer alloc = layerTransaction alloc' (syncToDiskIO . fst)
  where
    alloc' = do
      l <- alloc
      schema <- liftIO $ getLayerSchema (unLayer l)
      return (l, schema)


-- | GDAL < 1.11 only supports 1 geometry field and associates it to the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> maybeNewSpatialRefBorrowedHandle
          ({#call unsafe OGR_L_GetSpatialRef as ^#} p)
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
  -> GDALSource s (Maybe Fid, a)
executeSQL dialect query mSpatialFilter ds =
  sourceFromLayer
    (catchJust selectExc execute (throwBindingException . SQLQueryError))
    ({#call unsafe OGR_DS_ReleaseResultSet as ^#} (unDataSource ds) . unLayer)
  where
    execute =
      liftIO $
      liftM Layer $
      checkReturns (/=nullLayerH) $
      withMaybeGeometry mSpatialFilter $ \sFilter ->
      useAsEncodedCString query $ \sQuery ->
      withSQLDialect dialect $
        {#call OGR_DS_ExecuteSQL as ^#} (unDataSource ds) sQuery sFilter
    selectExc GDALException{gdalErrNum=AppDefined,..} = Just gdalErrMsg
    selectExc _                                       = Nothing

executeSQL_
  :: OGRFeature a
  => SQLDialect -> Text -> Maybe Geometry -> RODataSource s
  -> GDALSource s a
executeSQL_ dialect query mSpatialFilter =
  (=$= (CL.map snd)) . executeSQL dialect query mSpatialFilter

layerName :: Layer s t a -> GDAL s Text
layerName =
  liftIO . (peekEncodedCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

layerExtent :: Layer s t a -> GDAL s Envelope
layerExtent l = liftIO $ alloca $ \pE -> do
  checkOGRError ({#call OGR_L_GetExtent as ^#} (unLayer l) pE 1)
  peek pE

layerFeatureDef :: Layer s t a -> GDAL s FeatureDef
layerFeatureDef = liftIO . layerFeatureDefIO . unLayer

layerFeatureDefIO :: LayerH -> IO FeatureDef
layerFeatureDefIO pL = do
  gfd <- layerGeomFieldDef pL
  getLayerSchema pL >>= featureDefFromHandle gfd


getSpatialFilter :: Layer s t a -> GDAL s (Maybe Geometry)
getSpatialFilter l = liftIO $
  {#call unsafe OGR_L_GetSpatialFilter as ^#} (unLayer l) >>= maybeCloneGeometry

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
