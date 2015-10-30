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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
  , setLayerSpatialFilter

  , dataSourceLayerCount
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

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (isNothing, fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Vector as V

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (liftM, when, void, (>=>), (<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (
    MonadThrow(..)
  , MonadCatch
  , MonadMask
  , bracket
  , finally
  )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..), CChar(..), CLong(..))
#if GDAL_VERSION_MAJOR >= 2
import Foreign.C.Types (CLLong(..))
#endif
import Foreign.Ptr (Ptr, nullPtr)
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

newtype OGR s l a = OGR (GDAL s a)

deriving instance Functor (OGR s l)
deriving instance Applicative (OGR s l)
deriving instance Monad (OGR s l)
deriving instance MonadIO (OGR s l)
deriving instance MonadThrow (OGR s l)
deriving instance MonadCatch (OGR s l)
deriving instance MonadMask (OGR s l)
deriving instance MonadBase IO (OGR s l)
deriving instance MonadResource (OGR s l)

runOGR :: (forall l. OGR s l a )-> GDAL s a
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

type Driver = String

create :: Driver -> String -> OptionList -> GDAL s (RWDataSource s)
create driverName name options = newDataSourceHandle $ do
  pDr <- driverByName driverName
  withCString name $ \pName ->
    checkGDALCall checkIt
      (withOptionList options $ {#call OGR_Dr_CreateDataSource as ^#} pDr pName)
  where
    checkIt e p' | p'==nullDataSourceH = Just (fromMaybe dflt e)
    checkIt e _                        = e
    dflt = GDALException CE_Failure OpenFailed "OGR_Dr_CreateDataSource"

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
  :: forall s l a. OGRFeatureDef a
  => RWDataSource s -> ApproxOK -> OptionList -> GDAL s (RWLayer s l a)
createLayer ds = createLayerWithDef ds (featureDef (Proxy :: Proxy a))



createLayerWithDef
  :: RWDataSource s -> FeatureDef -> ApproxOK -> OptionList
  -> GDAL s (RWLayer s l a)
createLayerWithDef ds FeatureDef{..} approxOk options =
  liftM Layer $
  flip allocate (const (return ())) $
  useAsEncodedCString fdName $ \pName ->
  withMaybeSpatialReference (gfdSrs fdGeom) $ \pSrs ->
  withOptionList options $ \pOpts -> do
    pL <- checkGDALCall checkIt $
            {#call OGR_DS_CreateLayer as ^#} pDs pName pSrs gType pOpts
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
    checkOGRError . {#call OGR_L_SetFeature as ^#} pL
  where pL = unLayer layer

getFeature :: OGRFeature a => Layer s l t a -> Fid -> GDAL s (Maybe a)
getFeature layer (Fid fid) = liftIO $ do
  when (not (pL `layerHasCapability` RandomRead)) $
    throwBindingException (UnsupportedLayerCapability RandomRead)
  fDef <- layerFeatureDefIO pL
  liftM (fmap snd) $ featureFromHandle fDef $
    {#call OGR_L_GetFeature as ^#} pL (fromIntegral fid)
  where pL = unLayer layer

deleteFeature :: Layer s l t a -> Fid -> GDAL s ()
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

syncToDisk :: RWDataSource s -> GDAL s ()
syncToDisk =
  liftIO . checkOGRError . {#call OGR_DS_SyncToDisk as ^#} . unDataSource

syncLayerToDiskIO :: RWLayer s l a -> IO ()
syncLayerToDiskIO = checkOGRError . {#call OGR_L_SyncToDisk as ^#} . unLayer

syncLayerToDisk :: RWLayer s l a -> GDAL s ()
syncLayerToDisk = liftIO . syncLayerToDiskIO


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


sourceLayer
  :: OGRFeature a
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
  :: OGRFeature a => GDAL s (Layer s l t a) -> OGRSource s l a
sourceLayer_ = (=$= (CL.map snd)) . sourceLayer


conduitInsertLayer
  :: OGRFeature a
  => GDAL s (RWLayer s l a) -> OGRConduit s l (Maybe Fid, a) (Maybe Fid)
conduitInsertLayer = flip conduitFromLayer createIt
  where
    createIt (l, pFd) = awaitForever $ \(fid, feat) -> do
      fid' <- liftIO $ featureToHandle pFd fid feat $ \pF -> do
                checkOGRError ({#call OGR_L_CreateFeature as ^#} (unLayer l) pF)
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
        checkOGRError . {#call OGR_L_SetFeature as ^#} (unLayer l)


layerTransaction
  :: GDAL s (Layer s l t a, e)
  -> ((Layer s l t a, e) -> OGRConduit s l i o)
  -> OGRConduit s l i o
layerTransaction alloc inside = do
  alloc' <- lift (liftOGR (unsafeGDALToIO alloc))
  (rbKey, seed) <- allocate alloc' rollback
  liftIO $ checkOGRError $
    {#call OGR_L_StartTransaction as ^#} (unLayer (fst seed))
  addCleanup (const (release rbKey))
             (inside seed >> liftIO (commit rbKey seed))
  where
    free = closeLayer . fst
    commit rbKey seed = bracket (unprotect rbKey) (const (free seed)) $ \m -> do
      when (isNothing m) (error "layerTransaction: this should not happen")
      checkOGRError ({#call OGR_L_CommitTransaction as ^#} (unLayer (fst seed)))
      checkOGRError ({#call OGR_L_SyncToDisk as ^#} (unLayer (fst seed)))

    rollback seed =
      checkOGRError ({#call OGR_L_RollbackTransaction as ^#} (unLayer (fst seed)))
        `finally` free seed



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


-- | GDAL < 1.11 only supports 1 geometry field and associates it to the layer
layerGeomFieldDef :: LayerH -> IO GeomFieldDef
layerGeomFieldDef p =
  GeomFieldDef
    <$> liftM toEnumC ({#call unsafe OGR_L_GetGeomType	as ^#} p)
    <*> maybeNewSpatialRefBorrowedHandle
          ({#call unsafe OGR_L_GetSpatialRef as ^#} p)
    <*> pure True

dataSourceLayerCount :: DataSource s t -> GDAL s Int
dataSourceLayerCount = liftM fromIntegral
           . liftIO . {#call OGR_DS_GetLayerCount as ^#} . unDataSource

dataSourceName :: DataSource s t -> GDAL s String
dataSourceName =
  liftIO . (peekCString <=< {#call OGR_DS_GetName as ^#} . unDataSource)


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

layerName :: Layer s l t a -> GDAL s Text
layerName =
  liftIO . (peekEncodedCString <=< {#call unsafe OGR_L_GetName as ^#} . unLayer)

layerExtent :: Layer s l t a -> GDAL s Envelope
layerExtent l = liftIO $ alloca $ \pE -> do
  checkOGRError ({#call OGR_L_GetExtent as ^#} (unLayer l) pE 1)
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
