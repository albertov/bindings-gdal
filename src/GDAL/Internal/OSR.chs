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
  , isSameGeogCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , withSpatialReference
  , withMaybeSRAsCString
  , withMaybeSpatialReference
  , newSpatialRefHandle
  , newSpatialRefBorrowedHandle
  , maybeNewSpatialRefHandle
  , maybeNewSpatialRefBorrowedHandle
) where

{# context lib = "gdal" prefix = "OSR" #}

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch)
import Control.Monad (liftM, (>=>))

import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (
    ForeignPtr
  , withForeignPtr
  , newForeignPtr
  , newForeignPtr_
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Util
import GDAL.Internal.OGRError
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLConv (cplFree)

#include "ogr_srs_api.h"

{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance Show SpatialReference where
   show = toWkt

toWkt :: SpatialReference -> String
toWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe ExportToPrettyWkt as ^#} s' p 1

toProj4 :: SpatialReference -> String
toProj4 = exportWith {#call unsafe ExportToProj4 as ^#}

toXML :: SpatialReference -> String
toXML = exportWith fun
  where
    fun s' p = {#call unsafe ExportToXML as ^#} s' p (castPtr nullPtr)

exportWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CInt)
  -> SpatialReference
  -> String
exportWith fun s = unsafePerformIO $ alloca $ \ptr ->
  liftM (either (error "could not serialize SpatialReference") id) $
  checkOGRError
    (withSpatialReference s (\s' -> fun s' ptr))
    (do str <- peek ptr
        wkt <- peekCString str
        cplFree str
        return wkt)


foreign import ccall "ogr_srs_api.h &OSRDestroySpatialReference"
  c_destroySpatialReference :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle
  :: Ptr SpatialReference -> IO SpatialReference
newSpatialRefHandle = maybeNewSpatialRefHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefHandle
  :: Ptr SpatialReference -> IO (Maybe SpatialReference)
maybeNewSpatialRefHandle p
  | p==nullPtr = return Nothing
  | otherwise  = (Just . SpatialReference)
             <$> newForeignPtr c_destroySpatialReference p

newSpatialRefBorrowedHandle
  :: Ptr SpatialReference -> IO SpatialReference
newSpatialRefBorrowedHandle =
  maybeNewSpatialRefBorrowedHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefBorrowedHandle
  :: Ptr SpatialReference -> IO (Maybe SpatialReference)
maybeNewSpatialRefBorrowedHandle p
  | p==nullPtr = return Nothing
  | otherwise  = liftM (Just . SpatialReference) (newForeignPtr_ p)

emptySpatialRef :: IO SpatialReference
emptySpatialRef =
  {#call unsafe NewSpatialReference as ^#} nullPtr >>= newSpatialRefHandle

fromWkt, fromProj4, fromXML :: String -> Either OGRException SpatialReference
fromWkt s = unsafePerformIO $
  (withCString s $ \a ->
    fmap Right
      ({#call unsafe NewSpatialReference as ^#} a >>= newSpatialRefHandle))
        `catch` (return . Left)

fromProj4 = fromImporter importFromProj4
fromXML = fromImporter importFromXML

fromEPSG :: Int -> Either OGRException SpatialReference
fromEPSG = fromImporter importFromEPSG

fromImporter
  :: (SpatialReference -> a -> IO CInt)
  -> a
  -> Either OGRException SpatialReference
fromImporter f s = unsafePerformIO $ do
  r <- emptySpatialRef
  (do err <- liftM toEnumC $ f r s
      case err of
        None -> return $ Right r
        e    -> return $ Left (OGRException e "")) `catch` (return . Left)


{#fun ImportFromProj4 as ^
   {withSpatialReference* `SpatialReference', `String'} -> `CInt' id  #}

{#fun ImportFromEPSG as ^
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' id  #}

{#fun ImportFromXML as ^
   {withSpatialReference* `SpatialReference', `String'} -> `CInt' id  #}

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
withMaybeSRAsCString (Just srs) = withCString (toWkt srs)

withMaybeSpatialReference
  :: Maybe SpatialReference -> (Ptr SpatialReference -> IO a) -> IO a
withMaybeSpatialReference Nothing  = ($ nullPtr)
withMaybeSpatialReference (Just s) = withSpatialReference s
