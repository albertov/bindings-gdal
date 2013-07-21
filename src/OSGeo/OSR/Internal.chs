{-# LANGUAGE ForeignFunctionInterface , BangPatterns  #-}

module OSGeo.OSR.Internal (
    SpatialReference
  , fromWkt
  , fromProj4
  , fromEPSG
  , exportToPrettyWkt
) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (throw, catchJust, fromException)
import Control.Monad (liftM)

import Foreign.C.String (withCString, CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, freeHaskellFunPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr
                          ,mallocForeignPtrArray)
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
   show s = exportToPrettyWkt s False

exportToPrettyWkt :: SpatialReference -> Bool -> String
exportToPrettyWkt s simpl = unsafePerformIO $
    alloca $ \ptr -> do
      err <- withSpatialReference s $ \s' ->
               {#call unsafe OSRExportToPrettyWkt as ^#} s' ptr (fromBool simpl)
      case toEnumC err of
         None -> do str <- peek ptr
                    wkt <- peekCString str
                    {#call unsafe VSIFree as ^#} (castPtr str)
                    return wkt
         err' -> return "<error converting to pretty WKT"

   

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

fromWkt, fromProj4 :: String -> Either Error SpatialReference
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

{#fun unsafe OSRImportFromProj4 as importFromProj4
   {withSpatialReference* `SpatialReference', `String'} -> `CInt' id  #}

{#fun unsafe OSRImportFromEPSG as importFromEPSG
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' id  #}
