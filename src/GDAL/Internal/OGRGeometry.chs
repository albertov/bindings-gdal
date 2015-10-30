{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.OGRGeometry (
    GeometryType (..)
  , Geometry (..)
  , WkbByteOrder (..)
  , Envelope (..)
  , OGREnvelope

  , geomFromWkt
  , geomFromWkb
  , geomFromGml

  , geomToWkt
  , geomToWkb
  , geomToGml
  , geomToKml
  , geomToJson

  , geometrySpatialReference
  , geometryType
  , geometryEnvelope

  , transformWith
  , transformTo

  , withGeometry
  , withMaybeGeometry
  , cloneGeometry
  , maybeCloneGeometry
  , newGeometryHandle
  , maybeNewGeometryHandle
  , geomFromWktIO
  , geomFromWkbIO
  , geomToWktIO
  , geomToWkbIO
  , geomToGmlIO
  , geomToKmlIO
  , geomToJsonIO
) where

#include "ogr_api.h"

{#context lib = "gdal" prefix = "OGR_G_"#}

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throw, bracketOnError, catch)
import Control.Monad (liftM, when, (>=>))

import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Unsafe (
    unsafeUseAsCString
  , unsafeUseAsCStringLen
  )
import Data.Maybe (fromJust, fromMaybe)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..), CUChar(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr)
import Foreign.ForeignPtr (
    ForeignPtr
  , withForeignPtr
  , newForeignPtr
  , mallocForeignPtrBytes
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.OSR#}
import GDAL.Internal.CPLString (peekCPLString)
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.Util


data Envelope =
  Envelope {
    eMinX :: Double
  , eMinY :: Double
  , eMaxX :: Double
  , eMaxY :: Double
  } deriving (Eq, Show)

{#pointer *OGREnvelope->Envelope #}

instance Storable Envelope where
  sizeOf _    = {#sizeof OGREnvelope#}
  alignment _ = {#alignof OGREnvelope#}
  peek p =
    Envelope <$> liftM realToFrac ({#get OGREnvelope->MinX#} p)
             <*> liftM realToFrac ({#get OGREnvelope->MinY#} p)
             <*> liftM realToFrac ({#get OGREnvelope->MaxX#} p)
             <*> liftM realToFrac ({#get OGREnvelope->MaxY#} p)
  poke p Envelope{..} = do
    {#set OGREnvelope->MinX#} p (realToFrac eMinX)
    {#set OGREnvelope->MinY#} p (realToFrac eMinY)
    {#set OGREnvelope->MaxX#} p (realToFrac eMaxX)
    {#set OGREnvelope->MaxY#} p (realToFrac eMaxY)


{#enum OGRwkbGeometryType as GeometryType {upcaseFirstLetter}
  deriving (Eq,Show)#}

{#enum define WkbByteOrder
  { wkbXDR  as WkbXDR
  , wkbNDR  as WkbNDR
  } deriving (Eq, Show) #}

{#pointer OGRGeometryH as Geometry foreign newtype#}

cloneGeometry :: Ptr Geometry -> IO Geometry
cloneGeometry = maybeCloneGeometry >=> maybe (throw NullGeometry) return

maybeCloneGeometry :: Ptr Geometry -> IO (Maybe Geometry)
maybeCloneGeometry p
  | p==nullPtr = return Nothing
  | otherwise  = maybeNewGeometryHandle ({#call unsafe Clone as ^#} p)

withMaybeGeometry :: Maybe Geometry -> (Ptr Geometry -> IO a) -> IO a
withMaybeGeometry (Just g) = withGeometry g
withMaybeGeometry Nothing  = ($ nullPtr)

foreign import ccall "ogr_api.h &OGR_G_DestroyGeometry"
  c_destroyGeometry :: FunPtr (Ptr Geometry -> IO ())

newGeometryHandle
  :: (Ptr (Ptr Geometry) -> IO OGRError) -> IO (Either OGRError Geometry)
newGeometryHandle alloc = with nullPtr $ \pptr ->
  bracketOnError (go pptr) (const (freeIfNotNull pptr)) return
  where
    go pptr = do
      err <- alloc pptr
      if err /= None
        then freeIfNotNull pptr >> return (Left err)
        else do
          p <- peek pptr
          when (p==nullPtr) (throwBindingException NullGeometry)
          liftM (Right . Geometry) (newForeignPtr c_destroyGeometry p)
    freeIfNotNull pptr = do
      p <- peek pptr
      when (p /= nullPtr) ({#call unsafe OGR_G_DestroyGeometry as ^#} p)

maybeNewGeometryHandle
  :: IO (Ptr Geometry) -> IO (Maybe Geometry)
maybeNewGeometryHandle alloc =
  bracketOnError alloc freeIfNotNull $ \p -> do
    if p == nullPtr
      then return Nothing
      else liftM (Just . Geometry) (newForeignPtr c_destroyGeometry p)
  where
    freeIfNotNull p = do
      when (p /= nullPtr) ({#call unsafe OGR_G_DestroyGeometry as ^#} p)

geomFromWkb
  :: Maybe SpatialReference -> ByteString -> Either OGRError Geometry
geomFromWkb mSr = unsafePerformIO . geomFromWkbIO mSr
{-# NOINLINE geomFromWkb #-}

geomFromWkbIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError Geometry)
geomFromWkbIO mSrs bs =
  unsafeUseAsCStringLen bs $ \(sp, len) ->
  withMaybeSpatialReference mSrs $ \srs ->
  newGeometryHandle $ \gPtr ->
    liftM toEnumC $ {#call unsafe OGR_G_CreateFromWkb as ^#}
                      (castPtr sp) srs gPtr (fromIntegral len)

geomFromWkt
  :: Maybe SpatialReference -> ByteString -> Either OGRError Geometry
geomFromWkt mSrs = unsafePerformIO . geomFromWktIO mSrs
{-# NOINLINE geomFromWkt #-}

geomFromWktIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError Geometry)
geomFromWktIO mSrs bs =
  unsafeUseAsCString bs $ \sp ->
  with sp $ \spp ->
  withMaybeSpatialReference mSrs $ \srs ->
  newGeometryHandle $
    liftM toEnumC . {#call unsafe OGR_G_CreateFromWkt as ^#} spp srs

geomFromGml
  :: ByteString -> Either OGRError Geometry
geomFromGml = unsafePerformIO . geomFromGmlIO
{-# NOINLINE geomFromGml #-}

geomFromGmlIO
  :: ByteString -> IO (Either OGRError Geometry)
geomFromGmlIO bs =
  liftM (maybe (Left CorruptData) Right) $
  maybeNewGeometryHandle $
  unsafeUseAsCString bs $ {#call unsafe OGR_G_CreateFromGML as ^#}


geomToWktIO :: Geometry -> IO ByteString
geomToWktIO g =
  withGeometry g $ \gPtr ->
  peekCPLString $
    checkOGRError . {#call unsafe OGR_G_ExportToWkt as ^ #} gPtr

geomToWkt :: Geometry -> ByteString
geomToWkt = unsafePerformIO . geomToWktIO

geomToWkbIO :: WkbByteOrder -> Geometry -> IO ByteString
geomToWkbIO bo g = withGeometry g $ \gPtr -> do
  len <- liftM fromIntegral ({#call unsafe OGR_G_WkbSize as ^ #} gPtr)
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp $
    checkOGRError
      . {#call unsafe OGR_G_ExportToWkb as ^ #} gPtr (fromEnumC bo)
      . castPtr
  return $! PS fp 0 len

geomToWkb :: WkbByteOrder -> Geometry -> ByteString
geomToWkb bo = unsafePerformIO . geomToWkbIO bo


exportWith :: (Ptr Geometry -> IO CString) -> Geometry -> IO ByteString
exportWith f g =
  withGeometry g $ \gPtr ->
  peekCPLString $ \ptr ->
    checkGDALCall checkit (f gPtr) >>= poke ptr
  where
    checkit Nothing p | p/=nullPtr = Nothing
    checkit Nothing p | p==nullPtr =
      Just (GDALException CE_Failure AssertionFailed "exportWith: null ptr")
    checkit e _ = e

geomToKmlIO :: Geometry -> IO ByteString
geomToKmlIO =
  exportWith (flip {#call unsafe OGR_G_ExportToKML as ^ #} nullPtr)

geomToKml :: Geometry -> ByteString
geomToKml = unsafePerformIO . geomToKmlIO

geomToJsonIO :: Geometry -> IO ByteString
geomToJsonIO = exportWith {#call unsafe OGR_G_ExportToJson as ^ #}

geomToJson :: Geometry -> ByteString
geomToJson = unsafePerformIO . geomToJsonIO


geomToGmlIO :: Geometry -> IO ByteString
geomToGmlIO = exportWith {#call unsafe OGR_G_ExportToGML as ^ #}

geomToGml :: Geometry -> ByteString
geomToGml = unsafePerformIO . geomToGmlIO

geomEqIO :: Geometry -> Geometry -> IO Bool
geomEqIO a b = withGeometry a $ \aPtr -> withGeometry b $ \bPtr ->
  liftM toBool ({#call unsafe OGR_G_Equals as ^#} aPtr bPtr)

geometrySpatialReference
  :: Geometry -> Maybe SpatialReference
geometrySpatialReference g = unsafePerformIO $
  maybeNewSpatialRefBorrowedHandle $
    withGeometry g {#call unsafe OGR_G_GetSpatialReference as ^#}

geometryType
  :: Geometry -> GeometryType
geometryType g =
  unsafePerformIO $
  liftM toEnumC $
  withGeometry g $
    {#call unsafe OGR_G_GetGeometryType as ^#}

{#fun pure unsafe OGR_G_GetEnvelope as geometryEnvelope
  { `Geometry'
  , alloca- `Envelope' peek*
  } -> `()'#}

instance Show Geometry where
  show = unpack . geomToWkt

instance Eq Geometry where
  a == b = unsafePerformIO (geomEqIO a b)

transformWith :: Geometry -> CoordinateTransformation -> Maybe Geometry
transformWith g ct = unsafePerformIO $ do
  transformed <- withGeometry g cloneGeometry
  withCoordinateTransformation ct $ \pCt ->
    withGeometry transformed $ \pG -> do
      err <- liftM toEnumC ({#call unsafe OGR_G_Transform as ^#} pG pCt)
      return (if err==None then Just transformed else Nothing)

transformTo :: Geometry -> SpatialReference -> Maybe Geometry
transformTo g srs = unsafePerformIO $ do
  transformed <- withGeometry g cloneGeometry
  withSpatialReference srs $ \pSrs ->
    withGeometry transformed $ \pG -> do
      err <- liftM toEnumC ({#call unsafe OGR_G_TransformTo as ^#} pG pSrs)
      return (if err==None then Just transformed else Nothing)
