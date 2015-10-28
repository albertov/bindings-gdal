{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.OGRGeometry (
    GeometryType (..)
  , Geometry (..)
  , WkbByteOrder (..)
  , Envelope (..)
  , OGREnvelope

  , createFromWkt
  , createFromWkb
  , exportToWkt
  , exportToWkb
  , geometrySpatialReference
  , geometryType
  , geometryEnvelope

  , transformWith
  , transformTo

  , withGeometry
  , withMaybeGeometry
  , cloneGeometry
  , newGeometryHandle
  , createFromWktIO
  , createFromWkbIO
  , exportToWktIO
  , exportToWkbIO
) where

#include "ogr_api.h"

{#context lib = "gdal" prefix = "OGR_G_"#}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, void, (>=>))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Unsafe (
    unsafeUseAsCString
  , unsafeUseAsCStringLen
  , unsafePackMallocCStringLen
  )

import Foreign.C.Types (CInt(..), CDouble(..), CChar(..), CUChar(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr)
import Foreign.ForeignPtr (
    ForeignPtr
  , withForeignPtr
  , newForeignPtr
  )
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Utils (toBool)
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
cloneGeometry = ({#call unsafe Clone as ^#} >=> newGeometryHandle)


withMaybeGeometry :: Maybe Geometry -> (Ptr Geometry -> IO a) -> IO a
withMaybeGeometry (Just g) = withGeometry g
withMaybeGeometry Nothing  = ($ nullPtr)

foreign import ccall "ogr_api.h &OGR_G_DestroyGeometry"
  c_destroyGeometry :: FunPtr (Ptr Geometry -> IO ())

newGeometryHandle :: Ptr Geometry -> IO Geometry
newGeometryHandle p
  | p==nullPtr = throwBindingException NullGeometry
  | otherwise  = Geometry <$> newForeignPtr c_destroyGeometry p

createFromWkb
  :: Maybe SpatialReference -> ByteString -> Either OGRError Geometry
createFromWkb mSr = unsafePerformIO . createFromWkbIO mSr
{-# NOINLINE createFromWkb #-}

createFromWkbIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError Geometry)
createFromWkbIO mSrs bs =
  alloca $ \gPtr ->
  unsafeUseAsCStringLen bs $ \(sp, len) ->
  withMaybeSpatialReference mSrs $ \srs ->
    checkOGRError
      ({#call unsafe OGR_G_CreateFromWkb as ^#}
        (castPtr sp) srs gPtr (fromIntegral len))
      (peek gPtr >>= newGeometryHandle)

createFromWkt
  :: Maybe SpatialReference -> ByteString -> Either OGRError Geometry
createFromWkt mSrs = unsafePerformIO . createFromWktIO mSrs
{-# NOINLINE createFromWkt #-}

createFromWktIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRError Geometry)
createFromWktIO mSrs bs =
  alloca $ \gPtr ->
  alloca $ \spp ->
  unsafeUseAsCString bs $ \sp ->
  withMaybeSpatialReference mSrs $ \srs ->
    checkOGRError
      (poke spp sp >> {#call unsafe OGR_G_CreateFromWkt as ^#} spp srs gPtr)
      (peek gPtr >>= newGeometryHandle)



exportToWktIO :: Geometry -> IO ByteString
exportToWktIO g = withGeometry g $ \gPtr -> alloca $ \sPtrPtr -> do
  void $ {#call unsafe OGR_G_ExportToWkt as ^ #} (castPtr gPtr) sPtrPtr
  peekCPLString sPtrPtr

exportToWkt :: Geometry -> ByteString
exportToWkt = unsafePerformIO . exportToWktIO
{-# NOINLINE exportToWkt #-}

exportToWkbIO :: WkbByteOrder -> Geometry -> IO ByteString
exportToWkbIO bo g = withGeometry g $ \gPtr -> do
  len <- liftM fromIntegral ({#call unsafe OGR_G_WkbSize as ^ #} (castPtr gPtr))
  buf <- mallocBytes len
  void $ {#call unsafe OGR_G_ExportToWkb as ^ #}
           (castPtr gPtr) (fromEnumC bo) buf
  unsafePackMallocCStringLen (castPtr buf, len)

exportToWkb :: WkbByteOrder -> Geometry -> ByteString
exportToWkb bo = unsafePerformIO . exportToWkbIO bo
{-# NOINLINE exportToWkb #-}

geomEqIO :: Geometry -> Geometry -> IO Bool
geomEqIO a b = withGeometry a $ \aPtr -> withGeometry b $ \bPtr ->
  liftM toBool ({#call unsafe OGR_G_Equals as ^#} (castPtr aPtr) (castPtr bPtr))

geometrySpatialReference
  :: Geometry -> Maybe SpatialReference
geometrySpatialReference g = unsafePerformIO $
  withGeometry g $
    {#call unsafe OGR_G_GetSpatialReference as ^#} >=>
      maybeNewSpatialRefBorrowedHandle

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
  show = unpack . exportToWkt

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
