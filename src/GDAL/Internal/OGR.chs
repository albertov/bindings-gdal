{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.OGR (
    Error (..)
  , OGRException (..)
  , isOGRException
  , throwIfError
) where

import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Control.Exception (Exception, SomeException, throw, fromException)
import Control.Monad (liftM)

import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Util

#include "ogr_api.h"

data OGRException = OGRException Error String
     deriving (Show, Typeable)

instance Exception OGRException

isOGRException :: SomeException -> Bool
isOGRException e = isJust (fromException e :: Maybe OGRException)

{#enum define Error
  { OGRERR_NONE                      as None
  , OGRERR_NOT_ENOUGH_DATA           as NotEnoughData
  , OGRERR_NOT_ENOUGH_MEMORY         as NotEnoughMemory
  , OGRERR_UNSUPPORTED_GEOMETRY_TYPE as UnsupportedGeometryType
  , OGRERR_UNSUPPORTED_OPERATION     as UnsupportedOperation
  , OGRERR_CORRUPT_DATA              as CorruptData
  , OGRERR_FAILURE                   as Failure
  , OGRERR_UNSUPPORTED_SRS           as UnsupportedSRS
  , OGRERR_INVALID_HANDLE            as InvalidHandle
  } deriving (Eq, Show)#}

{#enum OGRwkbGeometryType as GeometryType {underscoreToCase}#}

instance Show GeometryType where
    show s = let s' = fromEnumC s in unsafePerformIO $ 
        {#call unsafe OGRGeometryTypeToName as ^#} s' >>= peekCString
        
throwIfError :: String -> IO CInt -> IO ()
throwIfError msg act = do
    e <- liftM toEnumC act
    case e of
      None -> return ()
      e'   -> throw $ OGRException e' msg
