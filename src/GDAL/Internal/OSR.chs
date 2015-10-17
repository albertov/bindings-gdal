{-# LANGUAGE ForeignFunctionInterface , BangPatterns  #-}

module GDAL.Internal.OSR (
    SpatialReference

  , fromWkt
  , fromProj4
  , fromEPSG
  , fromXML

  , toWkt
  , toProj4
  , toXML

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeoCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , withSpatialReference
  , withMaybeSRAsCString
) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch)
import Control.Monad (liftM)

import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Util
import GDAL.Internal.OGRError
import GDAL.Internal.CPLError hiding (None)

#include "ogr_srs_api.h"
#include "cpl_vsi.h"

{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance Show SpatialReference where
   show = either (\s -> "broken SpatialReference: " ++ show s) id . toWkt

toWkt :: SpatialReference -> Either OGRError String
toWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe OSRExportToPrettyWkt as ^#} s' p 1

toProj4 :: SpatialReference -> Either OGRError String
toProj4 = exportWith {#call unsafe OSRExportToProj4 as ^#}

toXML :: SpatialReference -> Either OGRError String
toXML = exportWith fun
  where
    fun s' p = {#call unsafe OSRExportToXML as ^#} s' p (castPtr nullPtr)

exportWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CInt)
  -> SpatialReference
  -> Either OGRError String
exportWith fun s = unsafePerformIO $ alloca $ \ptr ->
  checkOGRError
    (withSpatialReference s (\s' -> fun s' ptr))
    (do str <- peek ptr
        wkt <- peekCString str
        {#call unsafe VSIFree as ^#} (castPtr str)
        return wkt)


foreign import ccall unsafe "ogr_srs_api.h OSRNewSpatialReference"
  c_newSpatialRef :: CString -> IO (Ptr SpatialReference)

foreign import ccall "ogr_srs_api.h &OSRDestroySpatialReference"
  c_destroySpatialReference :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle :: Ptr SpatialReference
  -> IO SpatialReference
newSpatialRefHandle p
  | p==nullPtr = throwBindingException NullSpatialReference
  | otherwise  = SpatialReference <$> newForeignPtr c_destroySpatialReference p

emptySpatialRef :: IO SpatialReference
emptySpatialRef = c_newSpatialRef (castPtr nullPtr) >>= newSpatialRefHandle

fromWkt, fromProj4, fromXML :: String -> Either OGRError SpatialReference
fromWkt s = unsafePerformIO $
  (withCString s $ \a -> fmap Right (c_newSpatialRef a >>= newSpatialRefHandle))
    `catch`
  (\(OGRException e _) -> return (Left e))

fromProj4 = fromImporter importFromProj4
fromXML = fromImporter importFromXML

fromEPSG :: Int -> Either OGRError SpatialReference
fromEPSG = fromImporter importFromEPSG

fromImporter
  :: (SpatialReference -> a -> IO CInt)
  -> a
  -> Either OGRError SpatialReference
fromImporter f s = unsafePerformIO $ do
  r <- emptySpatialRef
  err <- liftM toEnumC $ f r s
  case err of
    None -> return $ Right r
    e    -> return $ Left e

{#fun OSRImportFromProj4 as importFromProj4
   {withSpatialReference* `SpatialReference', `String'} -> `CInt' id  #}

{#fun OSRImportFromEPSG as importFromEPSG
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' id  #}

{#fun OSRImportFromXML as importFromXML
   {withSpatialReference* `SpatialReference', `String'} -> `CInt' id  #}

{#fun pure unsafe OSRIsGeographic as isGeographic
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe OSRIsLocal as isLocal
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe OSRIsProjected as isProjected
   {withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe OSRIsSameGeogCS as isSameGeoCS
   { withSpatialReference * `SpatialReference'
   , withSpatialReference * `SpatialReference'} -> `Bool'#}

{#fun pure unsafe OSRIsSame as isSame
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
withMaybeSRAsCString (Just srs) = withCString (either (const "") id (toWkt srs))
