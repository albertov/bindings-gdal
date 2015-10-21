{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GDAL.Internal.OGRFeature (
    FieldType (..)
  , Field
  , Feature (..)
  , FeatureH
  , FieldDefnH (..)
  , FeatureDefnH (..)
  , Justification (..)

  , FeatureDef (..)
  , GeometryDef (..)
  , FieldDef (..)

  , fieldDef
  , fdGeom
  , featureToHandle
  , featureFromHandle
  , fieldByName
  , fieldByIndex
  , geometryByName
  , geometryByIndex

  , withFeatureH
  , withFieldDefnH
  , fieldDefFromHandle
  , featureDefFromHandle
) where

{#context lib = "gdal" prefix = "OGR" #}

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, (>=>), (<=<), when)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Text (Text)
import Data.Time.LocalTime (
    LocalTime(..)
  , TimeOfDay(..)
  , ZonedTime(..)
  , getCurrentTimeZone
  , minutesToTimeZone
  , utc
  )
import Data.Time ()
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
import GDAL.Internal.Util (
    toEnumC
  , fromEnumC
  , peekEncodedCString
  , useAsEncodedCString
  )
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.CPLError #}
{#import GDAL.Internal.OGRGeometry #}
{#import GDAL.Internal.OGRError #}

#include "ogr_core.h"
#include "ogr_api.h"
#include "cpl_string.h"

{#enum FieldType {} omit (OFTMaxType) deriving (Eq,Show,Read,Bounded) #}

{#enum Justification  {}
    omit (JustifyUndefined)
    with prefix = "OJ"
    add  prefix = "Justify"
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

data FieldDef
  = FieldDef {
      fldName  :: {-# UNPACK #-} !Text
    , fldType  :: {-# UNPACK #-} !FieldType
    , fldWidth :: {-# UNPACK #-} !(Maybe Int)
    , fldPrec  :: {-# UNPACK #-} !(Maybe Int)
    , fldJust  :: {-# UNPACK #-} !(Maybe Justification)
    } deriving (Show, Eq)

fieldDef :: FieldType -> Text -> FieldDef
fieldDef ftype name = FieldDef name ftype Nothing Nothing Nothing

data Feature
  = Feature {
      fId      :: {-# UNPACK #-} !Int64
    , fFields  :: {-# UNPACK #-} !(V.Vector Field)
    , fGeoms   :: {-# UNPACK #-} !(V.Vector Geometry)
    } deriving (Show)

data FeatureDef
  = FeatureDef {
      fdName    :: {-# UNPACK #-} !Text
    , fdFields  :: {-# UNPACK #-} !(V.Vector FieldDef)
    , fdGeoms   :: {-# UNPACK #-} !(V.Vector GeometryDef)
    } deriving (Show, Eq)

fdGeom :: FeatureDef -> Maybe GeometryDef
fdGeom = (V.!? 0) . fdGeoms

data GeometryDef
  = GeometryDef {
      gdName  :: {-# UNPACK #-} !Text
    , gdType  :: {-# UNPACK #-} !GeometryType
    , gdSrs   :: {-# UNPACK #-} !(Maybe SpatialReference)
    } deriving (Show, Eq)

{#pointer FeatureH foreign finalizer OGR_F_Destroy as ^ newtype#}
{#pointer FieldDefnH   newtype#}
{#pointer FeatureDefnH newtype#}


withFieldDefnH :: FieldDef -> (FieldDefnH -> IO a) -> IO a
withFieldDefnH FieldDef{..} f =
  useAsEncodedCString fldName $ \pName ->
  bracket ({#call unsafe OGR_Fld_Create as ^#} pName (fromEnumC fldType))
          ({#call unsafe OGR_Fld_Destroy as ^#}) (\p -> populate p >> f p)
  where
    populate h = do
      case fldWidth of
        Just w  -> {#call unsafe OGR_Fld_SetWidth as ^#} h (fromIntegral w)
        Nothing -> return ()
      case fldPrec of
        Just p  -> {#call unsafe OGR_Fld_SetPrecision as ^#} h (fromIntegral p)
        Nothing -> return ()
      case fldJust of
        Just j  -> {#call unsafe OGR_Fld_SetJustify as ^#} h (fromEnumC j)
        Nothing -> return ()

fieldDefFromHandle :: FieldDefnH -> IO FieldDef
fieldDefFromHandle p =
  FieldDef
    <$> ({#call unsafe OGR_Fld_GetNameRef as ^#} p >>= peekEncodedCString)
    <*> (liftM toEnumC ({#call unsafe OGR_Fld_GetType as ^#} p))
    <*> (liftM iToMaybe ({#call unsafe OGR_Fld_GetWidth as ^#} p))
    <*> (liftM iToMaybe ({#call unsafe OGR_Fld_GetPrecision as ^#} p))
    <*> (liftM jToMaybe ({#call unsafe OGR_Fld_GetJustify as ^#} p))
  where
    iToMaybe = (\v -> if v==0 then Nothing else Just (fromIntegral v))
    jToMaybe = (\j -> if j==0 then Nothing else Just (toEnumC j))

featureDefFromHandle :: FeatureDefnH -> IO FeatureDef
featureDefFromHandle p =
  FeatureDef
    <$> ({#call unsafe OGR_FD_GetName as ^#} p >>= peekEncodedCString)
    <*> getFields
    <*> pure V.empty
  where
    getFields = do
      nFields <- {#call unsafe OGR_FD_GetFieldCount as ^#} p
      V.generateM (fromIntegral nFields) $
        fieldDefFromHandle <=<
          ({#call OGR_FD_GetFieldDefn as ^#} p . fromIntegral)


featureToHandle :: FeatureDefnH -> Feature -> GDAL s FeatureH
featureToHandle = undefined

featureFromHandle :: FeatureH -> GDAL s Feature
featureFromHandle = undefined

fieldByName :: FeatureDefnH -> FeatureH -> ByteString -> GDAL s Field
fieldByName = undefined

fieldByIndex :: FeatureDefnH -> FeatureH -> Int -> GDAL s Field
fieldByIndex ftDef feature ix = liftIO $ do
  fDef <- {#call OGR_FD_GetFieldDefn as ^#} ftDef (fromIntegral ix)
  typ <- liftM toEnumC ({#call unsafe OGR_Fld_GetType as ^#} fDef)
  withFeatureH feature (getFieldBy typ  fDef (fromIntegral ix))

getFieldBy :: FieldType -> FieldDefnH -> CInt -> Ptr FeatureH -> IO Field

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
  (({#call unsafe OGR_F_GetFieldAsString as ^#} f ix) >>= peekEncodedCString)

getFieldBy OFTWideString fDef ix f = getFieldBy OFTString fDef ix f

getFieldBy OFTStringList _ ix f = liftM OGRStringList $ do
  ptr <- {#call unsafe OGR_F_GetFieldAsStringList as ^#} f ix
  nElems <- liftM fromIntegral ({#call unsafe CSLCount as ^#} ptr)
  V.generateM nElems (peekElemOff ptr >=> peekEncodedCString)

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

getFieldName :: FieldDefnH -> IO ByteString
getFieldName = {#call unsafe OGR_Fld_GetNameRef as ^#} >=> packCString


geometryByName :: FeatureH -> ByteString -> GDAL s Geometry
geometryByName = undefined

geometryByIndex :: FeatureH -> Int -> GDAL s Geometry
geometryByIndex = undefined
