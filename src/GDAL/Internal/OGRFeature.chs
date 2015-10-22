{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include "bindings.h"

module GDAL.Internal.OGRFeature (
    FieldType (..)
  , Field
  , Feature (..)
  , FeatureH
  , FieldDefnH (..)
  , FeatureDefnH (..)
  , Justification (..)

  , FeatureDef (..)
  , GeomFieldDef (..)
  , FieldDef (..)

  , fieldDef
  , featureToHandle
  , featureFromHandle

  , withFeatureH
  , withFieldDefnH
  , fieldDefFromHandle
  , featureDefFromHandle
#if MULTI_GEOM_FIELDS
  , GeomFieldDefnH (..)
  , withGeomFieldDefnH
#endif
) where

{#context lib = "gdal" prefix = "OGR" #}

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM, (>=>), (<=<), when)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (packCStringLen)
import Data.Int (Int32, Int64)
import Data.Monoid (mempty)
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

#include "gdal.h"
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
  = OGRInteger       {-# UNPACK #-} !Int32
  | OGRIntegerList   {-# UNPACK #-} !(St.Vector Int32)
  | OGRInteger64     {-# UNPACK #-} !Int64
  | OGRInteger64List {-# UNPACK #-} !(St.Vector Int64)
  | OGRReal          {-# UNPACK #-} !Double
  | OGRRealList      {-# UNPACK #-} !(St.Vector Double)
  | OGRString        {-# UNPACK #-} !Text
  | OGRStringList    {-# UNPACK #-} !(V.Vector Text)
  | OGRBinary        {-# UNPACK #-} !ByteString
  | OGRDateTime      {-# UNPACK #-} !ZonedTime
  | OGRDate          {-# UNPACK #-} !Day
  | OGRTime          {-# UNPACK #-} !TimeOfDay
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
    , fdGeom    :: {-# UNPACK #-} !GeomFieldDef
    , fdGeoms   :: {-# UNPACK #-} !(V.Vector GeomFieldDef)
    } deriving (Show, Eq)

data GeomFieldDef
  = GeomFieldDef {
      gfdName  :: {-# UNPACK #-} !Text
    , gfdType  :: {-# UNPACK #-} !GeometryType
    , gfdSrs   :: {-# UNPACK #-} !(Maybe SpatialReference)
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
    <$> getFieldName p
    <*> liftM toEnumC ({#call unsafe OGR_Fld_GetType as ^#} p)
    <*> liftM iToMaybe ({#call unsafe OGR_Fld_GetWidth as ^#} p)
    <*> liftM iToMaybe ({#call unsafe OGR_Fld_GetPrecision as ^#} p)
    <*> liftM jToMaybe ({#call unsafe OGR_Fld_GetJustify as ^#} p)
  where
    iToMaybe = (\v -> if v==0 then Nothing else Just (fromIntegral v))
    jToMaybe = (\j -> if j==0 then Nothing else Just (toEnumC j))

featureDefFromHandle :: GeomFieldDef -> FeatureDefnH -> IO FeatureDef
featureDefFromHandle gfd p =
  FeatureDef
    <$> ({#call unsafe OGR_FD_GetName as ^#} p >>= peekEncodedCString)
    <*> fieldDefsFromFeatureDefnH p
    <*> pure gfd
    <*> geomFieldDefsFromFeatureDefnH p
  where

fieldDefsFromFeatureDefnH :: FeatureDefnH -> IO (V.Vector FieldDef)
fieldDefsFromFeatureDefnH p = do
  nFields <- {#call unsafe OGR_FD_GetFieldCount as ^#} p
  V.generateM (fromIntegral nFields) $
    fieldDefFromHandle <=<
      ({#call unsafe OGR_FD_GetFieldDefn as ^#} p . fromIntegral)


geomFieldDefsFromFeatureDefnH :: FeatureDefnH -> IO (V.Vector GeomFieldDef)

#if MULTI_GEOM_FIELDS

{#pointer GeomFieldDefnH newtype#}

geomFieldDefsFromFeatureDefnH p = do
  nFields <- {#call unsafe OGR_FD_GetGeomFieldCount as ^#} p
  V.generateM (fromIntegral (nFields-1)) $
    gFldDef <=< ( {#call unsafe OGR_FD_GetGeomFieldDefn as ^#} p
                . (+1)
                . fromIntegral
                )
  where
    gFldDef g =
      GeomFieldDef
        <$> ({#call unsafe OGR_GFld_GetNameRef as ^#} g >>= peekEncodedCString)
        <*> liftM toEnumC ({#call unsafe OGR_GFld_GetType as ^#} g)
        <*> ({#call unsafe OGR_GFld_GetSpatialRef as ^#} g >>=
          maybeNewSpatialRefBorrowedHandle)

withGeomFieldDefnH :: GeomFieldDef -> (GeomFieldDefnH -> IO a) -> IO a
withGeomFieldDefnH GeomFieldDef{..} f =
  useAsEncodedCString gfdName $ \pName ->
  bracket ({#call unsafe OGR_GFld_Create as ^#} pName (fromEnumC gfdType))
          ({#call unsafe OGR_GFld_Destroy as ^#}) (\p -> populate p >> f p)
  where
    populate = withMaybeSpatialReference gfdSrs .
                 {#call unsafe OGR_GFld_SetSpatialRef as ^#}
#else
-- | GDAL < 1.11 only supports 1 geometry field and associates it the layer
geomFieldDefsFromFeatureDefnH = const (return V.empty)
#endif


featureToHandle :: FeatureDef   -> Feature -> GDAL s FeatureH
featureToHandle = undefined

featureFromHandle :: FeatureDef -> FeatureH -> GDAL s Feature
featureFromHandle = undefined


getFieldBy :: FieldType -> Text -> CInt -> Ptr FeatureH -> IO Field

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

#if (GDAL_VERSION_MAJOR >= 2)
getFieldBy OFTInteger64 _ ix f
  = liftM (OGRInteger64 . fromIntegral)
    ({#call unsafe OGR_F_GetFieldAsInteger64 as ^#} f ix)

getFieldBy OFTInteger64List _ ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsInteger64List as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
    copyBytes vP (buf :: Ptr CLong) (nElems * sizeOf (undefined :: CLong))
  liftM OGRInteger64List (St.unsafeFreeze (Stm.unsafeCast vec))
#endif

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

getFieldBy OFTWideString fname ix f = getFieldBy OFTString fname ix f

getFieldBy OFTStringList _ ix f = liftM OGRStringList $ do
  ptr <- {#call unsafe OGR_F_GetFieldAsStringList as ^#} f ix
  nElems <- liftM fromIntegral ({#call unsafe CSLCount as ^#} ptr)
  V.generateM nElems (peekElemOff ptr >=> peekEncodedCString)

getFieldBy OFTWideStringList fname ix f = getFieldBy OFTStringList fname ix f

getFieldBy OFTBinary _ ix f = alloca $ \lenP -> do
  buf <- liftM castPtr ({#call unsafe OGR_F_GetFieldAsBinary as ^#} f ix lenP)
  nElems <- peekIntegral lenP
  liftM OGRBinary (packCStringLen (buf, nElems))

getFieldBy OFTDateTime fname ix f
  = liftM OGRDateTime $ alloca $ \y -> alloca $ \m -> alloca $ \d ->
    alloca $ \h -> alloca $ \mn -> alloca $ \s -> alloca $ \tz -> do
      ret <- {#call unsafe OGR_F_GetFieldAsDateTime as ^#} f ix y m d h mn s tz
      when (ret==0) $ throwBindingException (FieldParseError fname)
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

getFieldBy OFTDate fname ix f
  = liftM (OGRDate . localDay . zonedTimeToLocalTime . unDateTime)
          (getFieldBy OFTDateTime fname ix f)

getFieldBy OFTTime fname ix f
  = liftM (OGRTime . localTimeOfDay . zonedTimeToLocalTime. unDateTime)
          (getFieldBy OFTDateTime fname ix f)

unDateTime :: Field -> ZonedTime
unDateTime (OGRDateTime f) = f
unDateTime _               = error "GDAL.Internal.OGRFeature.unDateTime"


peekIntegral :: (Storable a, Integral a, Num b) => Ptr a -> IO b
peekIntegral = liftM fromIntegral . peek

getFieldName :: FieldDefnH -> IO Text
getFieldName =
  {#call unsafe OGR_Fld_GetNameRef as ^#} >=> peekEncodedCString


geometryByName :: Text -> FeatureH -> GDAL s Geometry
geometryByName = undefined

geometryByIndex :: FeatureH -> Int -> GDAL s Geometry
geometryByIndex = undefined
