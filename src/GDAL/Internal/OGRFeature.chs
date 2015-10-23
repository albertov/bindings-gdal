{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include "bindings.h"

module GDAL.Internal.OGRFeature (
    OGRFeature (..)
  , OGRField   (..)
  , Fid (..)
  , FieldType (..)
  , Field (..)
  , Feature (..)
  , FeatureH (..)
  , FieldDefnH (..)
  , FeatureDefnH (..)
  , Justification (..)

  , FeatureDef (..)
  , GeomFieldDef (..)
  , FieldDef (..)

  , featureToHandle
  , featureFromHandle

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
import Control.Monad (liftM, liftM2, (>=>), (<=<), when, void)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32, Int64)
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))

import Data.Text (Text)
import Data.Time.LocalTime (
    LocalTime(..)
  , TimeOfDay(..)
  , TimeZone(..)
  , ZonedTime (..)
  , minutesToTimeZone
  , utc
  )
import Data.Time ()
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector as V

import Foreign.C.Types (
    CInt(..)
  , CLLong(..)
  , CDouble(..)
  , CChar(..)
  , CUChar(..)
  , CLong(..)
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes, toBool, fromBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
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

newtype Fid = Fid { unFid :: Int64 }
  deriving (Eq, Show, Num)

class OGRField a where
  fieldDef  :: Proxy a -> FieldDef
  toField   :: a  -> Field
  fromField :: Field -> Either Text a

class OGRFeature a where
  featureDef  :: Proxy a -> FeatureDef
  toFeature   :: a -> Feature
  fromFeature :: Feature -> Either Text a


{#enum FieldType {} omit (OFTMaxType) deriving (Eq,Show,Read,Bounded) #}

{#enum Justification  {}
    omit (JustifyUndefined)
    with prefix = "OJ"
    add  prefix = "Justify"
    deriving (Eq,Show,Read,Bounded) #}

data Field
  = OGRInteger       !Int32
  | OGRIntegerList   !(St.Vector Int32)
  | OGRInteger64     !Int64
  | OGRInteger64List !(St.Vector Int64)
  | OGRReal          !Double
  | OGRRealList      !(St.Vector Double)
  | OGRString        !Text
  | OGRStringList    !(V.Vector Text)
  | OGRBinary        !ByteString
  | OGRDateTime      !LocalTime !OGRTimeZone
  | OGRDate          !Day       !OGRTimeZone
  | OGRTime          !TimeOfDay !OGRTimeZone
  | OGRNullField
  deriving (Show, Eq)

data OGRTimeZone
  = UnknownTimeZone
  | LocalTimeZone
  | KnownTimeZone !TimeZone
  deriving (Eq, Show)

data FieldDef
  = FieldDef {
      fldType     :: !FieldType
    , fldWidth    :: !(Maybe Int)
    , fldPrec     :: !(Maybe Int)
    , fldJust     :: !(Maybe Justification)
    , fldNullable :: !Bool
    } deriving (Show, Eq)

data GeomFieldDef
  = GeomFieldDef {
      gfdType     :: !GeometryType
    , gfdSrs      :: !(Maybe SpatialReference)
    , gfdNullable :: !Bool
    } deriving (Show, Eq)

data Feature
  = Feature {
      fId      :: !(Maybe Fid)
    , fFields  :: !(V.Vector Field)
    , fGeom    :: !(Maybe Geometry)
    , fGeoms   :: !(V.Vector Geometry)
    } deriving (Show, Eq)

data FeatureDef
  = FeatureDef {
      fdName    :: !Text
    , fdFields  :: !(V.Vector (Text, FieldDef))
    , fdGeom    :: !GeomFieldDef
    , fdGeoms   :: !(V.Vector (Text, GeomFieldDef))
    } deriving (Show, Eq)



{#pointer FeatureH     newtype#}
{#pointer FieldDefnH   newtype#}
{#pointer FeatureDefnH newtype#}



withFieldDefnH :: Text -> FieldDef -> (FieldDefnH -> IO a) -> IO a
withFieldDefnH fldName FieldDef{..} f =
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
#if GDAL_VERSION_MAJOR >= 2
      {#call unsafe OGR_Fld_SetNullable as ^#} h (fromBool fldNullable)
#endif

fieldDefFromHandle :: FieldDefnH -> IO FieldDef
fieldDefFromHandle p =
  FieldDef
    <$> liftM toEnumC ({#call unsafe OGR_Fld_GetType as ^#} p)
    <*> liftM iToMaybe ({#call unsafe OGR_Fld_GetWidth as ^#} p)
    <*> liftM iToMaybe ({#call unsafe OGR_Fld_GetPrecision as ^#} p)
    <*> liftM jToMaybe ({#call unsafe OGR_Fld_GetJustify as ^#} p)
#if GDAL_VERSION_MAJOR >= 2
    <*> liftM toBool ({#call unsafe OGR_Fld_IsNullable as ^#} p)
#else
    <*> pure True
#endif
  where
    iToMaybe = (\v -> if v==0 then Nothing else Just (fromIntegral v))
    jToMaybe = (\j -> if j==0 then Nothing else Just (toEnumC j))

featureDefFromHandle :: GeomFieldDef -> FeatureDefnH -> IO FeatureDef
featureDefFromHandle gfd p = do
#if MULTI_GEOM_FIELDS
  gfields <- geomFieldDefsFromFeatureDefnH p
  let (gfd', gfields')
        -- should not happen but just in case
        | V.null gfields = (gfd, gfields)
        -- ignore layer definition since it doesn't carry correct nullability
        -- info
        | otherwise      = (snd (V.unsafeHead gfields), V.unsafeTail gfields)
#else
  let (gfd', gfields') = (gfd, V.empty)
#endif
  FeatureDef
    <$> ({#call unsafe OGR_FD_GetName as ^#} p >>= peekEncodedCString)
    <*> fieldDefsFromFeatureDefnH p
    <*> pure gfd'
    <*> pure gfields'

fieldDefsFromFeatureDefnH :: FeatureDefnH -> IO (V.Vector (Text, FieldDef))
fieldDefsFromFeatureDefnH p = do
  nFields <- {#call unsafe OGR_FD_GetFieldCount as ^#} p
  V.generateM (fromIntegral nFields) $ \i -> do
    fDef <- {#call unsafe OGR_FD_GetFieldDefn as ^#} p (fromIntegral i)
    liftM2 (,) (getFieldName fDef) (fieldDefFromHandle fDef)



#if MULTI_GEOM_FIELDS

geomFieldDefsFromFeatureDefnH
  :: FeatureDefnH -> IO (V.Vector (Text, GeomFieldDef))

{#pointer GeomFieldDefnH newtype#}

geomFieldDefsFromFeatureDefnH p = do
  nFields <- {#call unsafe OGR_FD_GetGeomFieldCount as ^#} p
  V.generateM (fromIntegral nFields) $
    gFldDef <=< ({#call unsafe OGR_FD_GetGeomFieldDefn as ^#} p . fromIntegral)
  where
    gFldDef g = do
      name <- {#call unsafe OGR_GFld_GetNameRef as ^#} g >>= peekEncodedCString
      gDef <- GeomFieldDef
              <$> liftM toEnumC ({#call unsafe OGR_GFld_GetType as ^#} g)
              <*> ({#call unsafe OGR_GFld_GetSpatialRef as ^#} g >>=
                    maybeNewSpatialRefBorrowedHandle)
#if (GDAL_VERSION_MAJOR >= 2)
              <*> liftM toBool ({#call unsafe OGR_GFld_IsNullable as ^#} g)
#else
              <*> pure True
#endif
      return (name, gDef)

withGeomFieldDefnH :: Text -> GeomFieldDef -> (GeomFieldDefnH -> IO a) -> IO a
withGeomFieldDefnH gfdName GeomFieldDef{..} f =
  useAsEncodedCString gfdName $ \pName ->
  bracket ({#call unsafe OGR_GFld_Create as ^#} pName (fromEnumC gfdType))
          ({#call unsafe OGR_GFld_Destroy as ^#}) (\p -> populate p >> f p)
  where
    populate p = do
      withMaybeSpatialReference gfdSrs $
        {#call unsafe OGR_GFld_SetSpatialRef as ^#} p
#if (GDAL_VERSION_MAJOR >= 2)
      {#call unsafe OGR_GFld_SetNullable as ^#} p (fromBool gfdNullable)
#endif

#endif -- MULTI_GEOM_FIELDS

nullFID :: Fid
nullFID = Fid ({#const OGRNullFID#})

featureToHandle :: FeatureDefnH -> Feature -> (FeatureH -> IO a) -> IO a
featureToHandle fdH Feature{..} act =
  bracket ({#call unsafe OGR_F_Create as ^#} fdH)
          ({#call unsafe OGR_F_Destroy as ^#}) $ \pF -> do
    case fId of
      Just (Fid fid) -> void $ {#call OGR_F_SetFID as ^#} pF (fromIntegral fid)
      Nothing        -> return ()
    imapM_ (\ix f -> setField f ix pF) fFields
    case fGeom of
      Just g  -> void $ withGeometry g ({#call OGR_F_SetGeometry as ^#} pF)
      Nothing -> return ()
    when (not (V.null fGeoms)) $
#if MULTI_GEOM_FIELDS
      flip imapM_ (G.tail fGeoms) $ \ix g ->
        void $ withGeometry g ({#call OGR_F_SetGeomField as ^#} pF (ix+1))
#else
      throwBindingException MultipleGeomFieldsNotSupported
#endif
    act pF

imapM f v  = V.mapM  (uncurry f) (V.zip (V.enumFromN 0 (V.length v)) v)
imapM_ f v = V.mapM_ (uncurry f) (V.zip (V.enumFromN 0 (V.length v)) v)


featureFromHandle :: FeatureDefnH -> IO FeatureH -> IO Feature
featureFromHandle fdH act =
  bracket act {#call unsafe OGR_F_Destroy as ^#} $ \pF -> do
    mFid <- liftM fromIntegral ({#call unsafe OGR_F_GetFID as ^#} pF)
    let fid = if mFid == nullFID then Nothing else Just mFid
    fieldDefs <- fieldDefsFromFeatureDefnH fdH
    fields <- flip imapM fieldDefs $ \i (fldName, f) -> do
      isSet <- liftM toBool ({#call unsafe OGR_F_IsFieldSet as ^#} pF i)
      if isSet
        then getField (fldType f) i pF >>=
              maybe (throwBindingException (FieldParseError fldName)) return
        else return OGRNullField
    geomRef <- {#call unsafe OGR_F_StealGeometry as ^#} pF
    geom <- if geomRef /= nullPtr
              then liftM Just (newGeometryHandle geomRef)
              else return Nothing
#if MULTI_GEOM_FIELDS
    nGeoms <- liftM fromIntegral
                ({#call unsafe OGR_FD_GetGeomFieldCount as ^#} fdH)
    geoms <- V.generateM (nGeoms-1) $ \ix ->
      {#call unsafe OGR_F_GetGeomFieldRef as ^#} pF (fromIntegral ix + 1) >>=
        cloneGeometry
#else
    let geoms = V.empty
#endif
    return Feature {fId=fid, fFields=fields, fGeom=geom, fGeoms=geoms}


-- ############################################################################
-- setField
-- ############################################################################

setField :: Field -> CInt -> FeatureH -> IO ()

setField (OGRInteger v) ix f =
  {#call OGR_F_SetFieldInteger as ^#} f (fromIntegral ix) (fromIntegral v)

setField (OGRIntegerList v) ix f =
  St.unsafeWith v $
    {#call OGR_F_SetFieldIntegerList as ^#} f (fromIntegral ix)
      (fromIntegral (St.length v)) . castPtr

#if (GDAL_VERSION_MAJOR >= 2)
setField (OGRInteger64 v) ix f =
  {#call OGR_F_SetFieldInteger64 as ^#} f (fromIntegral ix) (fromIntegral v)

setField (OGRInteger64List v) ix f =
  St.unsafeWith v $
    {#call OGR_F_SetFieldInteger64List as ^#} f (fromIntegral ix)
      (fromIntegral (St.length v)) . castPtr
#else
-- | TODO: check for overflow
setField (OGRInteger64 v)     ix f =
  setField (OGRInteger (fromIntegral v)) ix f

setField (OGRInteger64List v) ix f =
  setField (OGRIntegerList (St.map fromIntegral v)) ix f
#endif

setField (OGRReal v) ix f =
  {#call OGR_F_SetFieldDouble as ^#} f (fromIntegral ix) (realToFrac v)

setField (OGRRealList v) ix f =
  St.unsafeWith v $
    {#call OGR_F_SetFieldDoubleList as ^#} f (fromIntegral ix)
      (fromIntegral (St.length v)) . castPtr

setField (OGRString v) ix f = useAsEncodedCString v $
  {#call OGR_F_SetFieldString as ^#} f (fromIntegral ix)

setField (OGRStringList v) ix f =
  bracket createList {#call unsafe CSLDestroy as ^#} $
    {#call OGR_F_SetFieldStringList as ^#} f (fromIntegral ix)
  where
    createList = V.foldM' folder nullPtr v
    folder acc k =
      useAsEncodedCString k $ {#call unsafe CSLAddString as ^#} acc

setField (OGRBinary v) ix f = unsafeUseAsCStringLen v $ \(p,l) ->
  {#call OGR_F_SetFieldBinary as ^#}
    f (fromIntegral ix) (fromIntegral l) (castPtr p)

setField (OGRDateTime (LocalTime day (TimeOfDay h mn s)) tz) ix f =
  {#call OGR_F_SetFieldDateTime as ^#} f (fromIntegral ix)
  (fromIntegral y) (fromIntegral m) (fromIntegral d)
  (fromIntegral h) (fromIntegral mn) (truncate s) (fromOGRTimeZone tz)
  where (y, m, d) = toGregorian day

setField (OGRDate day tz) ix f =
  {#call OGR_F_SetFieldDateTime as ^#} f (fromIntegral ix)
  (fromIntegral y) (fromIntegral m) (fromIntegral d) 0 0 0 (fromOGRTimeZone tz)
  where (y, m, d) = toGregorian day

setField (OGRTime (TimeOfDay h mn s) tz) ix f =
  {#call OGR_F_SetFieldDateTime as ^#} f (fromIntegral ix) 0 0 0
  (fromIntegral h) (fromIntegral mn) (truncate s) (fromOGRTimeZone tz)

setField OGRNullField ix f =
  {#call unsafe OGR_F_UnsetField as ^#} f ix

-- ############################################################################
-- getField
-- ############################################################################

getField :: FieldType -> CInt -> FeatureH -> IO (Maybe Field)

getField OFTInteger ix f
  = liftM (Just . OGRInteger . fromIntegral)
    ({#call unsafe OGR_F_GetFieldAsInteger as ^#} f ix)

getField OFTIntegerList ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsIntegerList as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
    copyBytes vP (buf :: Ptr CInt) (nElems * sizeOf (undefined :: CInt))
  liftM (Just . OGRIntegerList) (St.unsafeFreeze (Stm.unsafeCast vec))

#if (GDAL_VERSION_MAJOR >= 2)
getField OFTInteger64 ix f
  = liftM (Just . OGRInteger64 . fromIntegral)
    ({#call unsafe OGR_F_GetFieldAsInteger64 as ^#} f ix)

getField OFTInteger64List ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsInteger64List as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
    copyBytes vP (buf :: Ptr CLLong) (nElems * sizeOf (undefined :: CLLong))
  liftM (Just . OGRInteger64List) (St.unsafeFreeze (Stm.unsafeCast vec))
#endif

getField OFTReal ix f
  = liftM (Just . OGRReal . realToFrac)
    ({#call unsafe OGR_F_GetFieldAsDouble as ^#} f ix)

getField OFTRealList ix f = alloca $ \lenP -> do
  buf <- {#call unsafe OGR_F_GetFieldAsDoubleList as ^#} f ix lenP
  nElems <- peekIntegral lenP
  vec <- Stm.new nElems
  Stm.unsafeWith vec $ \vP ->
     copyBytes vP (buf :: Ptr CDouble) (nElems * sizeOf (undefined :: CDouble))
  liftM (Just . OGRRealList) (St.unsafeFreeze (Stm.unsafeCast vec))

getField OFTString ix f = liftM (Just . OGRString)
  (({#call unsafe OGR_F_GetFieldAsString as ^#} f ix) >>= peekEncodedCString)

getField OFTWideString ix f = getField OFTString ix f

getField OFTStringList ix f = liftM (Just . OGRStringList) $ do
  ptr <- {#call unsafe OGR_F_GetFieldAsStringList as ^#} f ix
  nElems <- liftM fromIntegral ({#call unsafe CSLCount as ^#} ptr)
  V.generateM nElems (peekElemOff ptr >=> peekEncodedCString)

getField OFTWideStringList ix f = getField OFTStringList ix f

getField OFTBinary ix f = alloca $ \lenP -> do
  buf <- liftM castPtr ({#call unsafe OGR_F_GetFieldAsBinary as ^#} f ix lenP)
  nElems <- peekIntegral lenP
  liftM (Just . OGRBinary) (packCStringLen (buf, nElems))

getField OFTDateTime ix f =
  liftM (fmap (uncurry OGRDateTime)) (getDateTime ix f)

getField OFTDate ix f =
  liftM (fmap (\(LocalTime d _,tz) -> OGRDate d tz)) (getDateTime ix f)

getField OFTTime ix f =
  liftM (fmap (\(LocalTime _ t,tz) -> OGRTime t tz)) (getDateTime ix f)


getDateTime :: CInt -> FeatureH -> IO (Maybe (LocalTime, OGRTimeZone))
getDateTime ix f
  = alloca $ \y -> alloca $ \m -> alloca $ \d ->
    alloca $ \h -> alloca $ \mn -> alloca $ \s -> alloca $ \pTz -> do
      ret <- {#call unsafe OGR_F_GetFieldAsDateTime as ^#} f ix y m d h mn s pTz
      if ret == 0
        then return Nothing
        else do
          day <- fromGregorian <$> peekIntegral y
                               <*> peekIntegral m
                               <*> peekIntegral d
          tod <- TimeOfDay <$> peekIntegral h
                           <*> peekIntegral mn
                           <*> peekIntegral s
          iTz <- peekIntegral pTz
          return (Just (LocalTime day tod, toOGRTimezone iTz))

toOGRTimezone :: CInt -> OGRTimeZone
toOGRTimezone tz =
  case tz of
    0   -> UnknownTimeZone
    1   -> LocalTimeZone
    100 -> KnownTimeZone utc
    n   -> KnownTimeZone (minutesToTimeZone ((fromIntegral n - 100) * 15))

fromOGRTimeZone :: OGRTimeZone -> CInt
fromOGRTimeZone UnknownTimeZone    = 0
fromOGRTimeZone LocalTimeZone      = 1
fromOGRTimeZone (KnownTimeZone tz) = truncate ((mins / 15) + 100)
  where mins = fromIntegral (timeZoneMinutes tz) :: Double

peekIntegral :: (Storable a, Integral a, Num b) => Ptr a -> IO b
peekIntegral = liftM fromIntegral . peek

getFieldName :: FieldDefnH -> IO Text
getFieldName =
  {#call unsafe OGR_Fld_GetNameRef as ^#} >=> peekEncodedCString
