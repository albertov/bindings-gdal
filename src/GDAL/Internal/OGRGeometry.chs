{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module GDAL.Internal.OGRGeometry (
    GeometryType (..)
  , Geometry (..)
  , WkbByteOrder (..)
  , EnvelopeReal
  , Envelope (..)
  , OGREnvelope

  , envelopeSize

  , geomFromWkt
  , geomFromWkb
  , geomFromGml

  , geomToWkt
  , geomToWkb
  , geomToGml
  , geomToKml
  , geomToJson

  , geomSpatialReference
  , geomType
  , geomEnvelope

  , geomIntersects
  , geomEquals
  , geomDisjoint
  , geomTouches
  , geomCrosses
  , geomWithin
  , geomContains
  , geomOverlaps
  , geomSimplify
  , geomSimplifyPreserveTopology
  , geomSegmentize
  , geomBoundary
  , geomConvexHull
  , geomBuffer
  , geomIntersection
  , geomUnion
  , geomUnionCascaded
  , geomPointOnSurface
  , geomDifference
  , geomSymDifference
  , geomDistance
  , geomLength
  , geomArea
  , geomCentroid
  , geomIsEmpty
  , geomIsValid
  , geomIsSimple
  , geomIsRing
  , geomPolygonize

  , withGeometry
  , withGeometries
  , withMaybeGeometry
  , maybeCloneGeometryAndTransferOwnership
  , cloneGeometry
  , maybeCloneGeometry
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

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Monad.Catch (throwM, mask_, try)
import Control.Monad (liftM, (>=>))

import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Char8 (unpack, useAsCString)
import Data.ByteString.Unsafe (
    unsafeUseAsCString
  , unsafeUseAsCStringLen
  )
import Data.Typeable (Typeable)
import qualified Data.Vector.Storable as St

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..), CUChar(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr)
import Foreign.ForeignPtr (
    ForeignPtr
  , withForeignPtr
  , newForeignPtr
  , mallocForeignPtrBytes
  , touchForeignPtr
  )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

{#import GDAL.Internal.OGRError#}
{#import GDAL.Internal.OSR#}
import GDAL.Internal.Types
import GDAL.Internal.CPLString (peekCPLString)
import GDAL.Internal.CPLError (
    withQuietErrorHandler
  , throwBindingException
  )
import GDAL.Internal.Util


type EnvelopeReal = Envelope Double

{#pointer *OGREnvelope->EnvelopeReal #}

data Envelope a =
  Envelope {
    envelopeMin :: !(Pair a)
  , envelopeMax :: !(Pair a)
  } deriving (Eq, Show, Read, Functor, Typeable)

instance NFData a => NFData (Envelope a) where
  rnf (Envelope a b) = rnf a `seq` rnf b `seq` ()

envelopeSize :: Num a => Envelope a -> Pair a
envelopeSize w = liftA2 (-) (envelopeMax w) (envelopeMin w)
{-# INLINE envelopeSize #-}

instance Storable EnvelopeReal where
  sizeOf _    = {#sizeof OGREnvelope#}
  alignment _ = {#alignof OGREnvelope#}
  peek p =
    Envelope <$> ((:+:) <$> liftM realToFrac ({#get OGREnvelope->MinX#} p)
                        <*> liftM realToFrac ({#get OGREnvelope->MinY#} p))
             <*> ((:+:) <$> liftM realToFrac ({#get OGREnvelope->MaxX#} p)
                        <*> liftM realToFrac ({#get OGREnvelope->MaxY#} p))
  poke p (Envelope (eMinX :+: eMinY) (eMaxX :+: eMaxY))= do
    {#set OGREnvelope->MinX#} p (realToFrac eMinX)
    {#set OGREnvelope->MinY#} p (realToFrac eMinY)
    {#set OGREnvelope->MaxX#} p (realToFrac eMaxX)
    {#set OGREnvelope->MaxY#} p (realToFrac eMaxY)

instance Projectable EnvelopeReal where
  transformWith (Envelope e0 e1) ct = do
    [e0', e1'] <- ([e0, e1] :: St.Vector (Pair Double)) `transformWith` ct
    return (Envelope e0' e1')

{#enum OGRwkbGeometryType as GeometryType {upcaseFirstLetter}
  deriving (Eq,Show)#}

{#enum define WkbByteOrder
  { wkbXDR  as WkbXDR
  , wkbNDR  as WkbNDR
  } deriving (Eq, Show) #}

{#pointer OGRGeometryH as Geometry foreign newtype#}

withGeometries :: [Geometry] -> ([Ptr Geometry] -> IO a) -> IO a
withGeometries geoms f = do
  ret <- f $ map (\(Geometry fp) -> unsafeForeignPtrToPtr fp) geoms
  mapM_ (\(Geometry fp) -> touchForeignPtr fp) geoms
  return ret

instance Show Geometry where
  show = unpack . geomToWkt

instance Eq Geometry where
  a == b = a `geomEquals` b

cloneGeometry :: Ptr Geometry -> IO Geometry
cloneGeometry = maybeCloneGeometry >=> maybe (throwM NullGeometry) return

maybeCloneGeometry :: Ptr Geometry -> IO (Maybe Geometry)
maybeCloneGeometry p
  | p==nullPtr = return Nothing
  | otherwise  = maybeNewGeometryHandle ({#call unsafe Clone as ^#} p)

withMaybeGeometry :: Maybe Geometry -> (Ptr Geometry -> IO a) -> IO a
withMaybeGeometry (Just g) = withGeometry g
withMaybeGeometry Nothing  = ($ nullPtr)

maybeCloneGeometryAndTransferOwnership :: Maybe Geometry -> IO (Ptr Geometry)
maybeCloneGeometryAndTransferOwnership Nothing = return nullPtr
maybeCloneGeometryAndTransferOwnership (Just g) =
  withGeometry g {#call unsafe Clone as ^#}

foreign import ccall "ogr_api.h &OGR_G_DestroyGeometry"
  c_destroyGeometry :: FunPtr (Ptr Geometry -> IO ())

newGeometryHandle
  :: (Ptr (Ptr Geometry) -> IO OGRError) -> IO (Either OGRException Geometry)
newGeometryHandle io =
  try $
  with nullPtr $ \pptr ->
  mask_ $ do
    checkOGRError "newGeometryHandle" (liftM fromEnumC (io pptr))
    maybeNewGeometryHandle (peek pptr) >>= maybe throwNullGeom return
  where throwNullGeom = throwBindingException NullGeometry

maybeNewGeometryHandle
  :: IO (Ptr Geometry) -> IO (Maybe Geometry)
maybeNewGeometryHandle io =
  mask_ $ do
    p <- io
    if p == nullPtr
      then return Nothing
      else liftM (Just . Geometry) (newForeignPtr c_destroyGeometry p)

geomFromWkb
  :: Maybe SpatialReference -> ByteString -> Either OGRException Geometry
geomFromWkb mSr = unsafePerformIO . geomFromWkbIO mSr
{-# NOINLINE geomFromWkb #-}

geomFromWkbIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRException Geometry)
geomFromWkbIO mSrs bs =
  unsafeUseAsCStringLen bs $ \(sp, len) ->
  withMaybeSpatialReference mSrs $ \srs ->
  newGeometryHandle $ \gPtr ->
    liftM toEnumC $ {#call unsafe OGR_G_CreateFromWkb as ^#}
                      (castPtr sp) srs gPtr (fromIntegral len)

geomFromWkt
  :: Maybe SpatialReference -> ByteString -> Either OGRException Geometry
geomFromWkt mSrs = unsafePerformIO . geomFromWktIO mSrs
{-# NOINLINE geomFromWkt #-}

geomFromWktIO
  :: Maybe SpatialReference -> ByteString -> IO (Either OGRException Geometry)
geomFromWktIO mSrs bs =
  unsafeUseAsCString bs $ \sp ->
  with sp $ \spp ->
  withMaybeSpatialReference mSrs $ \srs ->
  newGeometryHandle $
    liftM toEnumC . {#call unsafe OGR_G_CreateFromWkt as ^#} spp srs

geomFromGml
  :: ByteString -> Either OGRException Geometry
geomFromGml = unsafePerformIO . geomFromGmlIO
{-# NOINLINE geomFromGml #-}

geomFromGmlIO
  :: ByteString -> IO (Either OGRException Geometry)
geomFromGmlIO bs =
  useAsCString bs $ \pS ->
  newGeometryHandle $ \gPtr -> do
    gP <- {#call unsafe OGR_G_CreateFromGML as ^#} pS
    if gP == nullPtr
      then return CorruptData
      else poke gPtr gP >> return None



geomToWktIO :: Geometry -> IO ByteString
geomToWktIO g =
  withGeometry g $ \gPtr ->
  peekCPLString $
    checkOGRError "geomToWkt" . {#call unsafe OGR_G_ExportToWkt as ^ #} gPtr


geomToWkt :: Geometry -> ByteString
geomToWkt = unsafePerformIO . geomToWktIO

geomToWkbIO :: WkbByteOrder -> Geometry -> IO ByteString
geomToWkbIO bo g = withGeometry g $ \gPtr -> do
  len <- liftM fromIntegral ({#call unsafe OGR_G_WkbSize as ^ #} gPtr)
  fp <- mallocForeignPtrBytes len
  withForeignPtr fp $
    checkOGRError "geomToWkb"
      . {#call unsafe OGR_G_ExportToWkb as ^ #} gPtr (fromEnumC bo)
      . castPtr
  return $! PS fp 0 len

geomToWkb :: WkbByteOrder -> Geometry -> ByteString
geomToWkb bo = unsafePerformIO . geomToWkbIO bo


exportWith :: (Ptr Geometry -> IO CString) -> Geometry -> IO ByteString
exportWith f g =
  withGeometry g $ \gPtr ->
  peekCPLString $ \ptr ->
  f gPtr >>= poke ptr

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

geomSpatialReference
  :: Geometry -> Maybe SpatialReference
geomSpatialReference g = unsafePerformIO $
  maybeNewSpatialRefBorrowedHandle $
    withGeometry g {#call unsafe OGR_G_GetSpatialReference as ^#}

geomType
  :: Geometry -> GeometryType
geomType g =
  unsafePerformIO $
  liftM toEnumC $
  withGeometry g $
    {#call unsafe OGR_G_GetGeometryType as ^#}

{#fun pure unsafe OGR_G_GetEnvelope as geomEnvelope
  { `Geometry'
  , alloca- `EnvelopeReal' peek*
  } -> `()'
  #}


instance Projectable Geometry where
  transformWith g ct =
    unsafePerformIO $
    withQuietErrorHandler $ do
      transformed <- withGeometry g cloneGeometry
      err <- liftM toEnumC $
             withGeometry transformed $
             withCoordinateTransformation ct .
             {#call unsafe OGR_G_Transform as ^#}
      return (if err==None then Just transformed else Nothing)

geomSimplify :: Double -> Geometry -> Maybe Geometry
geomSimplify =
  deriveGeomWith . flip {#call unsafe OGR_G_Simplify as ^#} . realToFrac

geomSimplifyPreserveTopology :: Double -> Geometry -> Maybe Geometry
geomSimplifyPreserveTopology =
  deriveGeomWith .
  flip {#call unsafe OGR_G_SimplifyPreserveTopology as ^#} .
  realToFrac

geomSegmentize :: Double -> Geometry -> Geometry
geomSegmentize maxLength geom = unsafePerformIO $ do
  segmentized <- withGeometry geom cloneGeometry
  withGeometry segmentized
    (flip {#call unsafe OGR_G_Segmentize as ^#} (realToFrac maxLength))
  return segmentized

{#fun pure unsafe OGR_G_Intersects as geomIntersects
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Equals as geomEquals
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Disjoint as geomDisjoint
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Touches as geomTouches
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Crosses as geomCrosses
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Within as geomWithin
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Contains as geomContains
  {`Geometry', `Geometry'} -> `Bool'
  #}

{#fun pure unsafe OGR_G_Overlaps as geomOverlaps
  {`Geometry', `Geometry'} -> `Bool'
  #}

geomBoundary :: Geometry -> Maybe Geometry
geomBoundary = deriveGeomWith {#call unsafe OGR_G_Boundary as ^#}

geomConvexHull :: Geometry -> Maybe Geometry
geomConvexHull = deriveGeomWith {#call unsafe OGR_G_ConvexHull as ^#}

geomBuffer :: Double -> Int -> Geometry -> Maybe Geometry
geomBuffer dist nQuads = deriveGeomWith $ \g ->
  {#call unsafe OGR_G_Buffer as ^#} g (realToFrac dist) (fromIntegral nQuads)

geomIntersection :: Geometry -> Geometry -> Maybe Geometry
geomIntersection = deriveGeomWith2 {#call unsafe OGR_G_Intersection as ^#}

geomUnion :: Geometry -> Geometry -> Maybe Geometry
geomUnion = deriveGeomWith2 {#call unsafe OGR_G_Union as ^#}

geomUnionCascaded :: Geometry -> Maybe Geometry
geomUnionCascaded = deriveGeomWith {#call unsafe OGR_G_UnionCascaded as ^#}

geomPointOnSurface :: Geometry -> Maybe Geometry
geomPointOnSurface = deriveGeomWith {#call unsafe OGR_G_PointOnSurface as ^#}

geomDifference :: Geometry -> Geometry -> Maybe Geometry
geomDifference = deriveGeomWith2 {#call unsafe OGR_G_Difference as ^#}

geomSymDifference :: Geometry -> Geometry -> Maybe Geometry
geomSymDifference = deriveGeomWith2 {#call unsafe OGR_G_SymDifference as ^#}

{#fun pure unsafe OGR_G_Distance as geomDistance
  {`Geometry', `Geometry'} -> `Double'
  #}

{#fun pure unsafe OGR_G_Length as geomLength {`Geometry'} -> `Double' #}

{#fun pure unsafe OGR_G_Area as geomArea {`Geometry'} -> `Double' #}

geomCentroid :: Geometry -> Maybe Geometry
geomCentroid geom = unsafePerformIO $ do
  mCentroid <- maybeNewGeometryHandle $
               {#call unsafe OGR_G_CreateGeometry as ^#} (fromEnumC WkbPoint)
  case mCentroid of
    Nothing       -> throwBindingException NullGeometry
    Just centroid -> do
      err <- liftM toEnumC $
             withGeometry geom $ \pGeom ->
             withGeometry centroid $
             {#call unsafe OGR_G_Centroid as ^#} pGeom
      case err of
        None -> return (Just centroid)
        _    -> return Nothing

{#fun pure unsafe OGR_G_IsEmpty as geomIsEmpty {`Geometry'} -> `Bool' #}

{#fun pure unsafe OGR_G_IsValid as geomIsValid {`Geometry'} -> `Bool' #}
{#fun pure unsafe OGR_G_IsSimple as geomIsSimple {`Geometry'} -> `Bool' #}
{#fun pure unsafe OGR_G_IsRing as geomIsRing {`Geometry'} -> `Bool' #}

geomPolygonize :: Geometry -> Maybe Geometry
geomPolygonize = deriveGeomWith {#call unsafe OGR_G_Polygonize as ^#}

deriveGeomWith
  :: (Ptr Geometry -> IO (Ptr Geometry)) -> Geometry -> Maybe Geometry
deriveGeomWith f =
  unsafePerformIO . maybeNewGeometryHandle . flip withGeometry f

deriveGeomWith2
  :: (Ptr Geometry -> Ptr Geometry -> IO (Ptr Geometry))
  -> Geometry -> Geometry -> Maybe Geometry
deriveGeomWith2 f g1 g2 =
  unsafePerformIO $
  maybeNewGeometryHandle $
  withGeometry g1 $ withGeometry g2 . f

