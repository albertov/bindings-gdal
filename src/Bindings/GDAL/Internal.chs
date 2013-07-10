{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.GDAL.Internal (
    GDALDataType (..)
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "gdal.h"

{# enum GDALDataType {underscoreToCase} deriving (Eq,Show) #}

