{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.OGRError (
    OGRError (..)
  , OGRException (..)

  , isOGRException
  , checkOGRError
) where

import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), SomeException, fromException)
import Control.Monad (liftM)

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Maybe (isJust)

import Foreign.C.Types (CInt(..))

import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.Util

#include "ogr_api.h"

data OGRException
  = OGRException !OGRError !String
  | NullDataSource
  | NullGeometry
  | NullSpatialReference
  | NullLayer
  | UnknownDriver     !String
  | InvalidLayerIndex !Int
  | InvalidLayerName  !Text
  | SQLQueryError     !Text
  | FieldParseError   !Text
  | MultipleGeomFieldsNotSupported
  | UnsupportedFieldType
  deriving (Show, Eq, Typeable)

instance NFData OGRException where
  rnf (OGRException e m) = rnf e `seq` rnf m `seq` ()
  rnf e                  = e `seq` ()

instance Exception OGRException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

isOGRException :: SomeException -> Bool
isOGRException e = isJust (fromException e :: Maybe OGRException)

{#enum define OGRError
  { OGRERR_NONE                      as None
  , OGRERR_NOT_ENOUGH_DATA           as NotEnoughData
  , OGRERR_NOT_ENOUGH_MEMORY         as NotEnoughMemory
  , OGRERR_UNSUPPORTED_GEOMETRY_TYPE as UnsupportedGeometryType
  , OGRERR_UNSUPPORTED_OPERATION     as UnsupportedOperation
  , OGRERR_CORRUPT_DATA              as CorruptData
  , OGRERR_FAILURE                   as Failure
  , OGRERR_UNSUPPORTED_SRS           as UnsupportedSRS
  , OGRERR_INVALID_HANDLE            as InvalidHandle
  } deriving (Eq, Show) #}

instance NFData OGRError where
  rnf e = e `seq`()


checkOGRError :: IO CInt -> IO a -> IO (Either OGRError a)
checkOGRError act ret = do
  err <- liftM toEnumC act
  case err of
    None -> liftM Right ret
    e    -> return (Left e)
