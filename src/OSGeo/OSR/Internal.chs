{-# LANGUAGE ForeignFunctionInterface , BangPatterns  #-}

module OSGeo.OSR.Internal (
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
) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throw, catchJust, fromException)
import Control.Monad (liftM)

import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, newForeignPtr
                          , mallocForeignPtrArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (toBool, fromBool)

import System.IO.Unsafe (unsafePerformIO)

import OSGeo.Util
import OSGeo.OGR 

#include "ogr_srs_api.h"
#include "cpl_vsi.h"

{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance Show SpatialReference where
   show = toWkt

toWkt :: SpatialReference -> String
toWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe OSRExportToPrettyWkt as ^#} s' p 1

toProj4 :: SpatialReference -> String
toProj4 = exportWith {#call unsafe OSRExportToProj4 as ^#}

toXML :: SpatialReference -> String
toXML = exportWith fun
  where
    fun s' p = {#call unsafe OSRExportToXML as ^#} s' p (castPtr nullPtr)

exportWith ::
     (Ptr SpatialReference -> Ptr CString -> IO (CInt))
  -> SpatialReference
  -> String
exportWith fun s = unsafePerformIO $
    alloca $ \ptr -> do
      err <- withSpatialReference s (\s' -> fun s' ptr)
      case toEnumC err of
         None -> do str <- peek ptr
                    wkt <- peekCString str
                    {#call unsafe VSIFree as ^#} (castPtr str)
                    return wkt
         err' -> throw $ OGRException Failure
               "OSGeo.OSR.Internal.exportWith: error exporting SpatialReference"
   

foreign import ccall unsafe "ogr_srs_api.h OSRNewSpatialReference"
  c_newSpatialRef :: CString -> IO (Ptr SpatialReference)

foreign import ccall "ogr_srs_api.h &OSRDestroySpatialReference"
  c_destroySpatialReference :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle :: Ptr SpatialReference
  -> IO SpatialReference
newSpatialRefHandle p
  = if p==nullPtr
       then throw $ OGRException Failure "could not allocate SpatialReference"
       else SpatialReference <$> newForeignPtr c_destroySpatialReference p

emptySpatialRef :: IO SpatialReference
emptySpatialRef = c_newSpatialRef (castPtr nullPtr) >>= newSpatialRefHandle

fromWkt, fromProj4, fromXML :: String -> Either Error SpatialReference
fromWkt s
  = unsafePerformIO $ catchJust guardIt createIt (return . Left)
  where
    createIt
      = withCString s $ \s' ->
        c_newSpatialRef s' >>= newSpatialRefHandle >>= return . Right
    guardIt e
      = case fromException e of
          Nothing                   -> Nothing
          Just (OGRException err _) -> Just err

fromProj4 = fromImporter importFromProj4
fromXML = fromImporter importFromXML

fromEPSG :: Int -> Either Error SpatialReference
fromEPSG = fromImporter importFromEPSG

fromImporter ::
  (SpatialReference -> a -> IO CInt)
  -> a
  -> Either Error SpatialReference
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
