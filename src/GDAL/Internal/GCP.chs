{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GDAL.Internal.GCP (
    GroundControlPoint (..)
  , GroundControlPointPtr
  , withGCPArrayLen
  , fromGCPArray
  , gcp
) where

#include "gdal.h"

import Data.ByteString.Char8 (empty, packCString)
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Coerce (coerce)

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad (liftM)

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (
    mallocArray0
  , allocaArray
  , peekArray
  , copyArray
  , advancePtr
  )
import Foreign.C.Types (CChar(..), CDouble(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Types (XY(..))

type GroundControlPointPtr = Ptr GroundControlPoint
{#pointer *GDAL_GCP as GroundControlPointPtr nocode#}

data GroundControlPoint =
  GCP {
    gcpId    :: ByteString
  , gcpInfo  :: ByteString
  , gcpPixel :: !(XY Double)
  , gcpPoint :: !(XY Double)
  , gcpElev  :: !Double
  } deriving (Eq, Show)

-- So the unsafe Storable instance doesn't escape the module
newtype GroundControlPointInternal = GCPI GroundControlPoint

gcp :: ByteString -> XY Double -> XY Double -> GroundControlPoint
gcp id_ px pt = GCP id_ empty px pt 0

withGCPArrayLen
  :: [GroundControlPoint] -> (Int -> GroundControlPointPtr -> IO a) -> IO a
withGCPArrayLen gcps act =
  allocaArray nPoints $ \pPoints ->
  bracket (mapM_ (uncurry (pokeElemOff pPoints)) (zip [0..] gcps'))
          (const (mapM_ (freeGCP . advancePtr pPoints) [0..nPoints-1]))
          (const (act nPoints (castPtr pPoints)))
  where
    freeGCP p = do
      {#get GDAL_GCP->pszId#} p >>= free
      {#get GDAL_GCP->pszInfo#} p >>= free
    nPoints  = length gcps
    gcps'    = coerce gcps :: [GroundControlPointInternal]

fromGCPArray :: Int -> GroundControlPointPtr -> IO [GroundControlPoint]
fromGCPArray i p = liftM coerce (peekArray i p')
  where p' = castPtr p :: Ptr GroundControlPointInternal


instance Storable GroundControlPointInternal where
  sizeOf _    = {#sizeof GDAL_GCP#}
  alignment _ = {#alignof GDAL_GCP#}
  poke p (GCPI GCP{..}) = do
    unsafeUseAsCStringLen gcpId $ \(pId,len) -> do
      ptr0 <- mallocArray0 len
      pokeElemOff ptr0 len 0
      copyArray ptr0 pId len
      {#set GDAL_GCP->pszId#} p ptr0
    unsafeUseAsCStringLen gcpInfo $ \(pInfo,len) -> do
      ptr0 <- mallocArray0 len
      pokeElemOff ptr0 len 0
      copyArray ptr0 pInfo len
      {#set GDAL_GCP->pszInfo#} p ptr0
    {#set GDAL_GCP->dfGCPPixel#} p (realToFrac (px gcpPixel))
    {#set GDAL_GCP->dfGCPLine#} p (realToFrac (py gcpPixel))
    {#set GDAL_GCP->dfGCPX#} p (realToFrac (px gcpPoint))
    {#set GDAL_GCP->dfGCPY#} p (realToFrac (py gcpPoint))
    {#set GDAL_GCP->dfGCPZ#} p (realToFrac gcpElev)
  peek p = liftM GCPI $
    GCP <$> ({#get GDAL_GCP->pszId#}   p >>= packCString)
        <*> ({#get GDAL_GCP->pszInfo#} p >>= packCString)
        <*> (XY <$> liftM realToFrac ({#get GDAL_GCP->dfGCPPixel#} p)
                <*> liftM realToFrac ({#get GDAL_GCP->dfGCPLine#} p))
       <*> (XY <$> liftM realToFrac ({#get GDAL_GCP->dfGCPX#} p)
               <*> liftM realToFrac ({#get GDAL_GCP->dfGCPY#} p))
       <*> liftM realToFrac ({#get GDAL_GCP->dfGCPZ#} p)
