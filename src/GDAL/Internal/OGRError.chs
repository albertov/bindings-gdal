{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module GDAL.Internal.OGRError (
    OGRError (..)
  , OGRException (..)
  , DriverCapability(..)
  , LayerCapability(..)
  , DataSourceCapability(..)

  , isOGRException
  , checkOGRError
  , gdalToOgrException
) where

import Control.Exception (Exception(..), SomeException, fromException)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Maybe (isJust)

import Foreign.C.Types (CInt(..))

import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.Util

#include "ogr_api.h"

data OGRException
  = OGRException { ogrErrType :: !OGRError
                 , ogrErrNum  :: !ErrorNum
                 , ogrErrMsg  :: !Text}
  | NullGeometry
  | NullSpatialReference
  | NullLayer
  | NullDataSource
  | NullCoordinateTransformation
  | UnexpectedNullFid
  | UnknownOGRDriver  !ByteString
  | InvalidLayerIndex !Int
  | InvalidLayerName  !Text
  | SQLQueryError     !Text
  | FieldParseError   !Text
  | FromFieldError    !Text
  | FromFeatureError  !Text
  | NoSuchField       !Text
  | MultipleGeomFieldsNotSupported
  | UnsupportedFieldType
  | UnsupportedLayerCapability !LayerCapability
  deriving (Show, Eq, Typeable)

instance Exception OGRException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

isOGRException :: SomeException -> Bool
isOGRException e = isJust (fromException e :: Maybe OGRException)

{#enum define OGRError
  { OGRERR_NONE                      as OGRNone
  , OGRERR_NOT_ENOUGH_DATA           as NotEnoughData
  , OGRERR_NOT_ENOUGH_MEMORY         as NotEnoughMemory
  , OGRERR_UNSUPPORTED_GEOMETRY_TYPE as UnsupportedGeometryType
  , OGRERR_UNSUPPORTED_OPERATION     as UnsupportedOperation
  , OGRERR_CORRUPT_DATA              as CorruptData
  , OGRERR_FAILURE                   as Failure
  , OGRERR_UNSUPPORTED_SRS           as UnsupportedSRS
  , OGRERR_INVALID_HANDLE            as InvalidHandle
  } deriving (Eq, Show) #}

checkOGRError :: Text -> IO CInt -> IO ()
checkOGRError msg = checkGDALCall_ $ \mExc r ->
  case (mExc, toEnumC r) of
    (Nothing, OGRNone) -> Nothing
    (Nothing, e)       -> Just (OGRException e AssertionFailed msg)
    (Just exc, e)      -> Just (gdalToOgrException e exc)
{-# INLINE checkOGRError #-}

gdalToOgrException :: OGRError -> GDALException -> OGRException
gdalToOgrException e exc = OGRException e (gdalErrNum exc) (gdalErrMsg exc)

data LayerCapability
  = RandomRead
  | SequentialWrite
  | RandomWrite
  | FastSpatialFilter
  | FastFeatureCount
  | FastGetExtent
  | CreateField
  | DeleteField
  | ReorderFields
  | AlterFieldDefn
  | Transactions
  | DeleteFeature
  | FastSetNextByIndex
  | StringsAsUTF8
  | IgnoreFields
  | CreateGeomField
  deriving (Eq, Show, Enum, Bounded)

data DataSourceCapability
  = CreateLayer
  | DeleteLayer
  | CreateGeomFieldAfterCreateLayer
  deriving (Eq, Show, Enum, Bounded)

data DriverCapability
  = CreateDataSource
  | DeleteDataSource
  deriving (Eq, Show, Enum, Bounded)
