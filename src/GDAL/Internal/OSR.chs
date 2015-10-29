{-# LANGUAGE ForeignFunctionInterface , BangPatterns  #-}

module GDAL.Internal.OSR (
    SpatialReference
  , CoordinateTransformation

  , fromWkt
  , fromProj4
  , fromEPSG
  , fromXML

  , toWkt
  , toProj4
  , toXML

  , coordinateTransformation

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeogCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , cleanup
  , fromWktIO
  , withSpatialReference
  , withMaybeSRAsCString
  , withMaybeSpatialReference
  , withCoordinateTransformation
  , newSpatialRefHandle
  , newSpatialRefBorrowedHandle
  , maybeNewSpatialRefHandle
  , maybeNewSpatialRefBorrowedHandle
) where

#include "ogr_srs_api.h"

{# context lib = "gdal" prefix = "OSR" #}

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch, bracketOnError, try)
import Control.Monad (liftM, (>=>), when, void)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.OGRError
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLString (peekCPLString)

{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance Show SpatialReference where
   show = show . toWkt

toWkt :: SpatialReference -> ByteString
toWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe ExportToPrettyWkt as ^#} s' p 1

toProj4 :: SpatialReference -> ByteString
toProj4 = exportWith {#call unsafe ExportToProj4 as ^#}

toXML :: SpatialReference -> ByteString
toXML = exportWith fun
  where
    fun s' p = {#call unsafe ExportToXML as ^#} s' p (castPtr nullPtr)

exportWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CInt)
  -> SpatialReference
  -> ByteString
exportWith fun s = unsafePerformIO $ peekCPLString $ \ptr ->
  checkOGRError (withSpatialReference s (\s' -> fun s' ptr))


foreign import ccall "ogr_srs_api.h &OSRRelease"
  c_release :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefHandle = maybeNewSpatialRefHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefHandle alloc = bracketOnError alloc freeIfNotNull go
  where
    go p
      | p==nullPtr = return Nothing
      | otherwise  = liftM (Just . SpatialReference) (newForeignPtr c_release p)
    freeIfNotNull p
      | p/=nullPtr = {#call unsafe OSRRelease as ^#} p
      | otherwise  = return ()

newSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefBorrowedHandle =
  maybeNewSpatialRefBorrowedHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefBorrowedHandle alloc = maybeNewSpatialRefHandle $ do
  p <- alloc
  when (p /= nullPtr) (void ({#call unsafe OSRReference as ^#} p))
  return p

emptySpatialRef :: IO SpatialReference
emptySpatialRef =
  newSpatialRefHandle ({#call unsafe NewSpatialReference as ^#} nullPtr)

fromWkt, fromProj4, fromXML
  :: ByteString -> Either OGRException SpatialReference
fromWkt = unsafePerformIO . (flip unsafeUseAsCString fromWktIO)
{-# NOINLINE fromWkt #-}

fromWktIO :: CString -> IO (Either OGRException SpatialReference)
fromWktIO a =
  (liftM Right
    (newSpatialRefHandle ({#call unsafe NewSpatialReference as ^#} a)))
  `catch` (return . Left)

fromProj4 = fromImporter importFromProj4
{-# NOINLINE fromProj4 #-}

fromXML = fromImporter importFromXML
{-# NOINLINE fromXML #-}

fromEPSG :: Int -> Either OGRException SpatialReference
fromEPSG = fromImporter importFromEPSG
{-# NOINLINE fromEPSG #-}

fromImporter
  :: (SpatialReference -> a -> IO CInt)
  -> a
  -> Either OGRException SpatialReference
fromImporter f s = unsafePerformIO $ do
  r <- emptySpatialRef
  try (checkOGRError (f r s) >> return r)


{#fun ImportFromProj4 as ^
   { withSpatialReference* `SpatialReference'
   , unsafeUseAsCString* `ByteString'} -> `CInt' id  #}

{#fun ImportFromEPSG as ^
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' id  #}

{#fun ImportFromXML as ^
   { withSpatialReference* `SpatialReference'
   , unsafeUseAsCString* `ByteString'} -> `CInt' id  #}

{#fun pure unsafe IsGeographic as ^
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsLocal as ^
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsProjected as ^
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSameGeogCS as ^
   { withSpatialReference * `SpatialReference'
   , withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSame as ^
   { withSpatialReference * `SpatialReference'
   , withSpatialReference * `SpatialReference'} -> `Bool'#}

instance Eq SpatialReference where
  (==) = isSame

getLinearUnits :: SpatialReference -> (Double, String)
getLinearUnits = getUnitsWith {#call unsafe OSRGetLinearUnits as ^#}

getAngularUnits :: SpatialReference -> (Double, String)
getAngularUnits = getUnitsWith {#call unsafe OSRGetAngularUnits as ^#}

getUnitsWith ::
     (Ptr SpatialReference -> Ptr CString -> IO CDouble)
  -> SpatialReference
  -> (Double, String)
getUnitsWith fun s = unsafePerformIO $
    alloca $ \p -> do
      value <- withSpatialReference s (\s' -> fun s' p)
      ptr <- peek p
      units <- peekCString ptr
      return (realToFrac value, units)

withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString Nothing    = ($ nullPtr)
withMaybeSRAsCString (Just srs) = unsafeUseAsCString (toWkt srs)

withMaybeSpatialReference
  :: Maybe SpatialReference -> (Ptr SpatialReference -> IO a) -> IO a
withMaybeSpatialReference Nothing  = ($ nullPtr)
withMaybeSpatialReference (Just s) = withSpatialReference s

{#pointer OGRCoordinateTransformationH as CoordinateTransformation
  foreign newtype#}

coordinateTransformation
  :: SpatialReference -> SpatialReference -> Maybe CoordinateTransformation
coordinateTransformation source target = unsafePerformIO $
  withSpatialReference source $ \pSource ->
  withSpatialReference target $ \pTarget -> do
    pCt <- {#call unsafe OCTNewCoordinateTransformation as ^#}
            pSource pTarget
    if pCt == nullPtr
      then return Nothing
      else liftM (Just . CoordinateTransformation)
                 (newForeignPtr c_destroyCT pCt)
{-# NOINLINE coordinateTransformation #-}

foreign import ccall "ogr_srs_api.h &OCTDestroyCoordinateTransformation"
  c_destroyCT :: FunPtr (Ptr CoordinateTransformation -> IO ())

{#fun OSRCleanup as cleanup {} -> `()'#}
