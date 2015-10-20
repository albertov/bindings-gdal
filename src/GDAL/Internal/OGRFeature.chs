{-# LANGUAGE ForeignFunctionInterface #-}

module GDAL.Internal.OGRFeature (
    FieldType (..)
  , Field
  , FeatureH
) where

import Data.Text (Text)
import Data.Time (UTCTime, DiffTime)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (
    ForeignPtr
  , FinalizerPtr
  , withForeignPtr
  )

#include "ogr_core.h"
#include "ogr_api.h"

{#enum OGRFieldType as FieldType {} with prefix = "OFT"
   deriving (Eq,Show,Read,Bounded) #}
 
data Field
  = OGRInteger     {-# UNPACK #-} !Int
  | OGRIntegerList {-# UNPACK #-} !(U.Vector Int)
  | OGRReal        {-# UNPACK #-} !Double
  | OGRRealList    {-# UNPACK #-} !(U.Vector Double)
  | OGRString      {-# UNPACK #-} !Text
  | OGRStringList  {-# UNPACK #-} !(V.Vector Text)
  | OGRBinary      {-# UNPACK #-} !ByteString
  | OGRDateTime    {-# UNPACK #-} !UTCTime
  | OGRTime        {-# UNPACK #-} !DiffTime
  deriving (Eq, Show)

data Feature
  = Feature { fFields     :: V.Vector (ByteString, Field) }

{#pointer OGRFeatureH as FeatureH foreign finalizer OGR_F_Destroy as ^ newtype#}

newtype FieldDefnH s = FieldDefnH (Ptr (FieldDefnH s))

{#pointer OGRFieldDefnH as FieldDefnH newtype nocode #}

newtype FeatureDefnH s = FeatureDefnH (Ptr (FeatureDefnH s))
{#pointer OGRFeatureDefnH as FeatureDefnH newtype nocode #}
