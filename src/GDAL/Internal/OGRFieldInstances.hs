{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.Typeable (Typeable, typeOf)

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

#define integralElem_(A,B,C) ogrField (A,B,C,fromIntegral,fromIntegral)
#define integralList_(A,B,C,to,from) \
  ogrField (A,B,C,(to . St.map fromIntegral),St.map fromIntegral . from)

#if GDAL_VERSION_MAJOR >= 2

#define integralElem(A) integralElem_(A,OFTInteger64,OGRInteger64)
#define integralList(A,to,from) \
  integralList_(A,OFTInteger64List,OGRInteger64List,to,from)

#else

#define integralElem(A) integralElem_(A,OFTInteger,OGRInteger)
#define integralList(A,to,from) \
  integralList_(A,OFTIntegerList,OGRIntegerList,to,from)

#endif

#define integral(A)                                                            \
integralElem(A)                                                                \
integralList([A], St.toList, St.fromList)                                      \
integralList(St.Vector A, id,         id)                                      \
integralList(U.Vector  A, St.convert, St.convert)                              \
integralList(V.Vector  A, St.convert, St.convert)

#define realElem(A) ogrField (A,OFTReal,OGRReal,realToFrac,realToFrac)
#define realList(A,to,from) \
  ogrField (A,OFTRealList,OGRRealList,(to . St.map realToFrac),St.map realToFrac . from)

#define real(A)                                                                \
realElem(A)                                                                    \
realList([A], St.toList, St.fromList)                                          \
realList(St.Vector A, id,         id)                                          \
realList(U.Vector  A, St.convert, St.convert)                                  \
realList(V.Vector  A, St.convert, St.convert)

--
-- OGRField instances
--

defaultFromField :: forall a. Typeable a => Field -> Either Text a
defaultFromField f = Left ("Unexpected '" <> typeName <> "' field: " <> tShow f)
  where
    tShow :: Show b => b -> Text
    tShow    = pack . show
    typeName = tShow (typeOf (undefined :: a))


instance OGRField a => OGRField (Maybe a) where
  fieldDef _             = (fieldDef (Proxy :: Proxy a)) {fldNullable = True}
  toField Nothing        = OGRNullField
  toField (Just a)       = toField a
  fromField OGRNullField = Right Nothing
  fromField a            = fmap Just (fromField a)


integral(Int)
integral(Int16)
integral(Int32)
integral(Int64)
integral(Word)
integral(Word16)
integral(Word32)
integral(Word64)

real(Float)
real(Double)

ogrField(Text,OFTString,OGRString,id,id)
ogrField([Text],OFTStringList,OGRStringList,V.toList,V.fromList)
ogrField(V.Vector Text,OFTStringList,OGRStringList,id,id)

ogrField(String,OFTString,OGRString,unpack,pack)
ogrField([String],OFTStringList,OGRStringList,(V.toList . V.map unpack),(V.map pack . V.fromList))
ogrField(V.Vector String,OFTStringList,OGRStringList,(V.convert . V.map unpack),(V.map pack . V.convert))

ogrField(ByteString,OFTBinary,OGRBinary,id,id)
