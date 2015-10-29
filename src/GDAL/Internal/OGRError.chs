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
) where

import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), SomeException, fromException)
import Control.Monad (liftM, when)

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
  | UnexpectedNullFid
  | UnknownDriver     !String
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

instance NFData OGRException where
  rnf (OGRException e n m) = rnf e `seq` rnf n `seq` rnf m `seq` ()
  rnf e                    = e `seq` ()

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

checkOGRError :: IO CInt -> IO ()
checkOGRError = checkGDALCall_ $ \mExc r ->
  case (mExc, toEnumC r) of
    (Nothing, None) -> Nothing
    (Nothing, e)    -> Just (OGRException e AssertionFailed "checkOGRError")
    (Just exc, e)   -> Just (OGRException e (gdalErrNum exc) (gdalErrMsg exc))
{-# INLINE checkOGRError #-}


instance NFData OGRError where
  rnf e = e `seq`()

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
