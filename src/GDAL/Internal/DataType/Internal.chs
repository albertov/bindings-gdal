module GDAL.Internal.DataType.Internal (DataType(..)) where

#include "gdal.h"

import GDAL.Internal.Util (fromEnumC)

import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Ptr

import System.IO.Unsafe (unsafePerformIO)


------------------------------------------------------------------------------
-- DataType
------------------------------------------------------------------------------

{#enum GDALDataType as DataType {} omit (GDT_TypeCount)
  deriving (Eq,Ord,Bounded) #}

instance Show DataType where
  show d = unsafePerformIO $
    {#call unsafe GDALGetDataTypeName as ^#} (fromEnumC d) >>= peekCString
