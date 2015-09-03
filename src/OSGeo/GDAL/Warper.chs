{-# LANGUAGE ForeignFunctionInterface #-}

module OSGeo.GDAL.Warper (
    ResampleAlg (..)
  , reprojectImage
) where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr, nullPtr)
import OSGeo.OSR (SpatialReference, toWkt)
import OSGeo.GDAL.Internal ( Dataset, RWDataset, GDALType, withDataset
                           , throwIfError)
import OSGeo.Util (fromEnumC)

#include "gdal.h"
#include "gdalwarper.h"

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} deriving (Eq,Read,Show) #}

foreign import ccall unsafe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset t a) -- ^Source dataset
  -> CString           -- ^Source proj (WKT)
  -> Ptr (RWDataset a) -- ^Dest dataset
  -> CString           -- ^Dest proj (WKT)
  -> CInt              -- ^Resample alg
  -> CDouble           -- ^Memory limit
  -> CDouble           -- ^Max error
  -> Ptr ()            -- ^Progress func (unused)
  -> Ptr ()            -- ^Progress arg (unused)
  -> Ptr ()            -- ^warp options (unused)
  -> IO CInt


reprojectImage
  :: GDALType a
  => Dataset t a
  -> Maybe SpatialReference
  -> RWDataset a
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Int
  -> Int
  -> IO ()
reprojectImage srcDs srcSr dstDs dstSr alg memLimit maxError
  | memLimit < 0 = error "reprojectImage: memLimit < 0"
  | maxError < 0 = error "reprojectImage: maxError < 0"
  | otherwise
  = throwIfError "reprojectImage: GDALReprojectImage retuned error" $
    withDataset srcDs $ \sDsPtr -> withDataset dstDs $ \dDsPtr ->
    withMaybeSRAsCString srcSr $ \sSr -> withMaybeSRAsCString dstSr $ \dSr ->
      c_reprojectImage sDsPtr sSr dDsPtr dSr (fromEnumC alg)
                       (fromIntegral memLimit) (fromIntegral maxError)
                       nullPtr nullPtr nullPtr


withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString Nothing f = f nullPtr
withMaybeSRAsCString (Just srs) f = withCString (toWkt srs) f
