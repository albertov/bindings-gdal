{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GDAL.Internal.OGRFeature (
    FieldType (..)
  , Field
  , Feature (..)
  , FeatureH

  , featureToHandle
  , featureFromHandle
  , fieldByName
  , fieldByIndex
  , geometryByName
  , geometryByIndex

  , withFeatureH

) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, (>=>), when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Text (Text)
import Data.Text.Foreign (peekCStringLen)
import Data.Time.LocalTime (
    LocalTime(..)
  , TimeOfDay(..)
  , ZonedTime(..)
  , getCurrentTimeZone
  , minutesToTimeZone
  , utc
  )
import Data.Time.Calendar (Day, fromGregorian)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (packCStringLen, packCString)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector as V

import Foreign.C.Types (CInt(..), CDouble(..), CChar(..), CUChar(..))
import Foreign.ForeignPtr (
    ForeignPtr
  , FinalizerPtr
  , withForeignPtr
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf, peek, peekElemOff)

import GDAL.Internal.Types
import GDAL.Internal.Util (toEnumC)
{#import GDAL.Internal.CPLError #}
{#import GDAL.Internal.OGRGeometry #}
{#import GDAL.Internal.OGRError #}

#include "ogr_core.h"
#include "ogr_api.h"
#include "cpl_string.h"

{#enum OGRFieldType as FieldType {} omit (OFTMaxType)
    deriving (Eq,Show,Read,Bounded) #}

data Field
  = OGRInteger     {-# UNPACK #-} !Int
  | OGRIntegerList {-# UNPACK #-} !(St.Vector Int)
  | OGRReal        {-# UNPACK #-} !Double
  | OGRRealList    {-# UNPACK #-} !(St.Vector Double)
  | OGRString      {-# UNPACK #-} !Text
  | OGRStringList  {-# UNPACK #-} !(V.Vector Text)
  | OGRBinary      {-# UNPACK #-} !ByteString
  | OGRDateTime    {-# UNPACK #-} !ZonedTime
  | OGRDate        {-# UNPACK #-} !Day
  | OGRTime        {-# UNPACK #-} !TimeOfDay
  deriving (Show)

data Feature
  = Feature {
      fId         :: Int64
    , fFields     :: V.Vector (ByteString, Field)
    , fGeometries :: V.Vector (ByteString, Geometry)
    } deriving (Show)

{#pointer OGRFeatureH as FeatureH foreign finalizer OGR_F_Destroy as ^ newtype#}

newtype FieldDefnH s = FieldDefnH (Ptr (FieldDefnH s))

{#pointer OGRFieldDefnH as FieldDefnH newtype nocode #}

newtype FeatureDefnH s = FeatureDefnH (Ptr (FeatureDefnH s))
{#pointer OGRFeatureDefnH as FeatureDefnH newtype nocode #}


featureToHandle :: FeatureDefnH s -> Feature -> GDAL s FeatureH
featureToHandle = undefined

featureFromHandle :: FeatureDefnH s -> FeatureH -> GDAL s Feature
featureFromHandle = undefined

fieldByName :: FeatureDefnH s -> FeatureH -> ByteString -> GDAL s Field
fieldByName = undefined

fieldByIndex :: FeatureDefnH s -> FeatureH -> Int -> GDAL s Field
fieldByIndex ftDef feature ix = liftIO $ do
  fDef <- c_getFieldDefn ftDef (fromIntegral ix)
  typ <- liftM toEnumC (c_getFieldType fDef)
  withFeatureH feature (getFieldBy typ  fDef (fromIntegral ix))

getFieldBy :: FieldType -> FieldDefnH s -> CInt -> Ptr FeatureH -> IO Field

getFieldBy OFTInteger _ ix f
  = liftM (OGRInteger . fromIntegral)
    ({#call unsafe OGR_F_GetFieldAsInteger as ^#} f ix)

getFieldBy OFTIntegerList _ ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsIntegerList as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
    copyBytes vP (buf :: Ptr CInt) (nElems * sizeOf (undefined :: CInt))
  liftM OGRIntegerList (St.unsafeFreeze (Stm.unsafeCast vec))

getFieldBy OFTReal _ ix f
  = liftM (OGRReal . realToFrac)
    ({#call unsafe OGR_F_GetFieldAsDouble as ^#} f ix)

getFieldBy OFTRealList _ ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsDoubleList as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
     copyBytes vP (buf :: Ptr CDouble) (nElems * sizeOf (undefined :: CDouble))
  liftM OGRRealList (St.unsafeFreeze (Stm.unsafeCast vec))

getFieldBy OFTString _ ix f = liftM OGRString
  (({#call unsafe OGR_F_GetFieldAsString as ^#} f ix) >>= peekCString)

getFieldBy OFTWideString fDef ix f = getFieldBy OFTString fDef ix f

getFieldBy OFTStringList _ ix f = liftM OGRStringList $ do
  ptr <- {#call unsafe OGR_F_GetFieldAsStringList as ^#} f ix
  nElems <- liftM fromIntegral ({#call unsafe CSLCount as ^#} ptr)
  V.generateM nElems (peekElemOff ptr >=> peekCString)

getFieldBy OFTWideStringList fDef ix f = getFieldBy OFTStringList fDef ix f

getFieldBy OFTBinary _ ix f = alloca $ \lenP -> do
  buf <- liftM castPtr ({#call unsafe OGR_F_GetFieldAsBinary as ^#} f ix lenP)
  nElems <- peekIntegral lenP
  liftM OGRBinary (packCStringLen (buf, nElems))

getFieldBy OFTDateTime fDef ix f
  = liftM OGRDateTime $ alloca $ \y -> alloca $ \m -> alloca $ \d ->
    alloca $ \h -> alloca $ \mn -> alloca $ \s -> alloca $ \tz -> do
      ret <- {#call unsafe OGR_F_GetFieldAsDateTime as ^#} f ix y m d h mn s tz
      when (ret==0) $
        getFieldName fDef >>= throwBindingException . FieldParseError
      day <- fromGregorian <$> peekIntegral y
                           <*> peekIntegral m
                           <*> peekIntegral d
      tod <- TimeOfDay <$> peekIntegral h
                       <*> peekIntegral mn
                       <*> peekIntegral s
      let lt = return . ZonedTime (LocalTime day tod)
      tzV <- peekIntegral tz
      case tzV of
        -- Unknown timezone, assume utc
        0   -> lt utc
        1   -> getCurrentTimeZone >>= lt
        100 -> lt utc
        n   -> lt (minutesToTimeZone ((n-100) * 15))

getFieldBy OFTDate fDef ix f
  = liftM (OGRDate . localDay . zonedTimeToLocalTime . unDateTime)
          (getFieldBy OFTDateTime fDef ix f)

getFieldBy OFTTime fDef ix f
  = liftM (OGRTime . localTimeOfDay . zonedTimeToLocalTime. unDateTime)
          (getFieldBy OFTDateTime fDef ix f)

unDateTime :: Field -> ZonedTime
unDateTime (OGRDateTime f) = f
unDateTime _               = error "GDAL.Internal.OGRFeature.unDateTime"


peekIntegral :: (Storable a, Integral a, Num b) => Ptr a -> IO b
peekIntegral = liftM fromIntegral . peek

foreign import ccall unsafe "OGR_Fld_GetType"
  c_getFieldType :: FieldDefnH s -> IO CInt

foreign import ccall unsafe "OGR_Fld_GetNameRef"
  c_getFieldName :: FieldDefnH s -> IO (Ptr CChar)

getFieldName :: FieldDefnH s -> IO ByteString
getFieldName = c_getFieldName >=> packCString

foreign import ccall unsafe "OGR_FD_GetFieldDefn"
  c_getFieldDefn :: FeatureDefnH s -> CInt -> IO (FieldDefnH s)

peekCString :: Ptr CChar -> IO Text
peekCString p = len 0 >>= (\l -> peekCStringLen (p,l))
  where
    len !n = peekElemOff p n >>= (\v -> if v==0 then return n else len (n+1))

geometryByName :: FeatureH -> ByteString -> GDAL s Geometry
geometryByName = undefined

geometryByIndex :: FeatureH -> Int -> GDAL s Geometry
geometryByIndex = undefined
