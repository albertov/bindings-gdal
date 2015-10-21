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

  , createFromWktIO
  , createFromWkbIO
  , exportToWktIO
  , exportToWkbIO

  , createFromWkt
  , createFromWkb
  , exportToWkt
  , exportToWkb

  , withGeometry
  , withMaybeGeometry
  , cloneGeometry
) where

{#context lib = "gdal" prefix = "OGR_G_"#}

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
{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.OSR#}
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLConv (cplFree)
import GDAL.Internal.Util

#include "ogr_api.h"

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

peekAndPack :: Ptr CString -> IO ByteString
peekAndPack pptr = do
  p <- liftM castPtr (peek pptr) :: IO (Ptr Word8)
  let findLen !n = do
        v <- peek (p `plusPtr` n) :: IO Word8
        if v==0 then return n else findLen (n+1)
  len <- findLen 0
  unsafePackCStringFinalizer p len (cplFree p)


exportToWktIO :: Geometry -> IO ByteString
exportToWktIO g = withGeometry g $ \gPtr -> alloca $ \sPtrPtr -> do
  void $ {#call OGR_G_ExportToWkt as ^ #} (castPtr gPtr) sPtrPtr
  peekAndPack sPtrPtr

exportToWkt :: Geometry -> ByteString
exportToWkt = unsafePerformIO . exportToWktIO

exportToWkbIO :: WkbByteOrder -> Geometry -> IO ByteString
exportToWkbIO bo g = withGeometry g $ \gPtr -> do
  len <- liftM fromIntegral ({#call OGR_G_WkbSize as ^ #} (castPtr gPtr))
  buf <- mallocBytes len
  void $ {#call OGR_G_ExportToWkb as ^ #} (castPtr gPtr) (fromEnumC bo) buf
  unsafePackMallocCStringLen (castPtr buf, len)

exportToWkb :: WkbByteOrder -> Geometry -> ByteString
exportToWkb bo = unsafePerformIO . exportToWkbIO bo

geomEqIO :: Geometry -> Geometry -> IO Bool
geomEqIO a b = withGeometry a $ \aPtr -> withGeometry b $ \bPtr ->
  liftM toBool ({#call OGR_G_Equals as ^#} (castPtr aPtr) (castPtr bPtr))

instance Show Geometry where
  show = unpack . exportToWkt

instance Eq Geometry where
  a == b = unsafePerformIO (geomEqIO a b)
