{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module GDAL.Internal.OGRFieldInstances () where

#include "bindings.h"

import Data.ByteString (ByteString)
import Data.Int
import Data.Word
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Typeable (Typeable, typeOf)

import Data.Vector.Generic (convert)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import GDAL.Internal.OGRFeature

#define ogrField(ty,oft,tyCon,to,from)                                         \
instance OGRField (ty) where {                                                 \
  fieldDef _          = FieldDef oft Nothing Nothing Nothing False             \
; toField             = tyCon . from                                           \
; fromField (tyCon v) = Right (to v)                                           \
; fromField f         = defaultFromField f};

#define integralElem(A,B,C) ogrField (A,B,C,fromIntegral,fromIntegral)
#define integralList(A,B,C,to,from) \
  ogrField (A,B,C,(to . St.map fromIntegral),St.map fromIntegral . from)


#define integral(A)                                                            \
integralElem(A,OFTInteger,OGRInteger)                                          \
integralList([A],OFTIntegerList,OGRIntegerList,St.toList,St.fromList)          \
integralList(St.Vector A,OFTIntegerList,OGRIntegerList,id,id)                  \
integralList(U.Vector A,OFTIntegerList,OGRIntegerList,convert,convert)         \
integralList(V.Vector A,OFTIntegerList,OGRIntegerList,convert,convert)

#define integral64(A)                                                          \
integralElem(A,OFTInteger64,OGRInteger64)                                      \
integralList([A],OFTInteger64List,OGRInteger64List,St.toList,St.fromList)      \
integralList(St.Vector A,OFTInteger64List,OGRInteger64List,id,id)              \
integralList(U.Vector A,OFTInteger64List,OGRInteger64List,convert,convert)     \
integralList(V.Vector A,OFTInteger64List,OGRInteger64List,convert,convert)

#define realElem(A) ogrField (A,OFTReal,OGRReal,realToFrac,realToFrac)
#define realList(A,to,from) ogrField (A,OFTRealList,OGRRealList                \
                                     ,(to . St.map realToFrac)                 \
                                     ,St.map realToFrac . from)

#define real(A)                                                                \
realElem(A)                                                                    \
realList([A], St.toList, St.fromList)                                          \
realList(St.Vector A, id,         id)                                          \
realList(U.Vector  A, convert, convert)                                        \
realList(V.Vector  A, convert, convert)

--
-- OGRField instances
--

defaultFromField :: forall a. Typeable a => Field -> Either Text a
defaultFromField f = Left ("Unexpected '" <> typeName <> "' field: " <> tShow f)
  where
    typeName = tShow (typeOf (undefined :: a))

tShow :: Show b => b -> Text
tShow = pack . show

instance OGRField a => OGRField (Maybe a) where
  fieldDef _             = (fieldDef (Proxy :: Proxy a)) {fldNullable = True}
  toField Nothing        = OGRNullField
  toField (Just a)       = toField a
  fromField OGRNullField = Right Nothing
  fromField a            = fmap Just (fromField a)

#if SUPPORTS_WORD_FIELDS
integral64(Int)
integral64(Word)
#endif

#if SUPPORTS_64_BIT_INT_FIELDS
integral64(Int64)
integral64(Word64)
#endif

integral(Int8)
integral(Word8)
integral(Int16)
integral(Word16)
integral(Int32)
integral(Word32)

real(Float)
real(Double)

ogrField(Text,OFTString,OGRString,id,id)
ogrField([Text],OFTStringList,OGRStringList,V.toList,V.fromList)
ogrField(V.Vector Text,OFTStringList,OGRStringList,id,id)

ogrField(String,OFTString,OGRString,unpack,pack)
ogrField([String],OFTStringList,OGRStringList,(V.toList . V.map unpack),(V.map pack . V.fromList))
ogrField(V.Vector String,OFTStringList,OGRStringList,(V.convert . V.map unpack),(V.map pack . V.convert))

ogrField(ByteString,OFTBinary,OGRBinary,id,id)

instance OGRField UTCTime where
  fieldDef _ =
    FieldDef OFTDateTime Nothing Nothing Nothing False
  toField u  =
    OGRDateTime (utcToLocalTime utc u) (KnownTimeZone utc)
  fromField (OGRDateTime lt (KnownTimeZone tz)) =
    Right (localTimeToUTC tz lt)
  fromField (OGRDateTime _ ogrtz) =
    Left ("UTCTime: Unexpected timezone '" <> tShow ogrtz <> "'")
  fromField f = defaultFromField f

instance OGRField LocalTime where
  fieldDef _ =
    FieldDef OFTDateTime Nothing Nothing Nothing False
  toField u  =
    OGRDateTime u LocalTimeZone
  fromField (OGRDateTime lt LocalTimeZone) =
    Right lt
  fromField (OGRDateTime _ ogrtz) =
    Left ("LocalTime: Unexpected timezone '" <> tShow ogrtz <> "'")
  fromField f = defaultFromField f

instance OGRField ZonedTime where
  fieldDef _ =
    FieldDef OFTDateTime Nothing Nothing Nothing False
  toField (ZonedTime lt tz)  =
    OGRDateTime lt (KnownTimeZone tz)
  fromField (OGRDateTime lt (KnownTimeZone tz)) =
    Right (ZonedTime lt tz)
  fromField (OGRDateTime _ ogrtz) =
    Left ("ZonedTime: Unexpected timezone '" <> tShow ogrtz <> "'")
  fromField f = defaultFromField f

ogrField(Day,OFTDate,OGRDate,id,id)
ogrField(TimeOfDay,OFTTime,OGRTime,id,id)
ogrField(DiffTime,OFTTime,OGRTime,timeOfDayToTime,timeToTimeOfDay)
