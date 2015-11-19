{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType

  , Reader
  , Writer
  , Indexer

  , convertGType
  , sizeOfDataType

  , gdtByte
  , gdtUInt16
  , gdtUInt32
  , gdtInt16
  , gdtInt32
  , gdtFloat32
  , gdtFloat64
  , gdtCInt16
  , gdtCInt32
  , gdtCFloat32
  , gdtCFloat64
  , gdtUnknown
) where

#include "bindings.h"

import Control.Arrow ((&&&))
import Control.Monad.Primitive
import Control.Monad.ST (runST)

import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.MachDeps

import Data.Int (Int8, Int16, Int32)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)

import Foreign.C.Types

import GHC.Base

import Unsafe.Coerce (unsafeCoerce)

import GDAL.Internal.Types.Pair (Pair(..))


#if MIN_VERSION_base(4,8,0)
import Data.Complex (Complex((:+)), realPart, imagPart)
#else
import Data.Complex (Complex((:+)))
realPart, imagPart :: Complex t -> t
realPart (a :+ _) = a
imagPart (_ :+ a) = a
#endif

newtype DataType = DataType Int
  deriving (Eq, Show) -- FIXME: implement a better Show instance

{-
instance Show DataType where
  show (DataType d) = unsafeInlineIO $
    {#call unsafe GetDataTypeName as ^#} (fromIntegral d) >>= peekCString
-}

instance Enum DataType where
  toEnum = DataType

  fromEnum (DataType a) = a

instance Bounded DataType where
  minBound = gdtByte
  maxBound = gdtCFloat64

gdtByte :: DataType
gdtByte = DataType GDT_BYTE

gdtUInt16 :: DataType
gdtUInt16 = DataType GDT_UINT16

gdtUInt32 :: DataType
gdtUInt32 = DataType GDT_UINT32

gdtInt16 :: DataType
gdtInt16 = DataType GDT_INT16

gdtInt32 :: DataType
gdtInt32 = DataType GDT_INT32

gdtFloat32 :: DataType
gdtFloat32 = DataType GDT_FLOAT32

gdtFloat64 :: DataType
gdtFloat64 = DataType GDT_FLOAT64

gdtCInt16 :: DataType
gdtCInt16 = DataType GDT_CINT16

gdtCInt32 :: DataType
gdtCInt32 = DataType GDT_CINT32

gdtCFloat32 :: DataType
gdtCFloat32 = DataType GDT_CFLOAT32

gdtCFloat64 :: DataType
gdtCFloat64 = DataType GDT_CFLOAT64

gdtUnknown :: DataType
gdtUnknown = DataType GDT_UNKNOWN

sizeOfDataType :: DataType -> Int
sizeOfDataType dt
  | dt == gdtByte     = sIZEOF_WORD8
  | dt == gdtUInt16   = sIZEOF_WORD16
  | dt == gdtUInt32   = sIZEOF_WORD32
  | dt == gdtInt16    = sIZEOF_INT16
  | dt == gdtInt32    = sIZEOF_INT32
  | dt == gdtFloat32  = sIZEOF_FLOAT
  | dt == gdtFloat64  = sIZEOF_DOUBLE
  | dt == gdtCInt16   = sIZEOF_INT16 * 2
  | dt == gdtCInt32   = sIZEOF_INT32 * 2
  | dt == gdtCFloat32 = sIZEOF_FLOAT * 2
  | dt == gdtCFloat64 = sIZEOF_DOUBLE * 2
  | otherwise         = error "sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}


------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------

type Reader s a = MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
type Writer s a = MutableByteArray# s -> Int# -> a -> State# s -> State# s
type Indexer a  = ByteArray# -> Int# -> a

class Eq a  => GDALType a where
  dataType :: Proxy a -> DataType

  gWrite :: DataType -> Writer s a
  gWrite !dt p# i# v
    | dt == gdtByte     = writeWith (undefined :: Word8) gToIntegral
    | dt == gdtUInt16   = writeWith (undefined :: Word16) gToIntegral
    | dt == gdtUInt32   = writeWith (undefined :: Word32) gToIntegral
    | dt == gdtInt16    = writeWith (undefined :: Int16) gToIntegral
    | dt == gdtInt32    = writeWith (undefined :: Int32) gToIntegral
    | dt == gdtFloat32  = writeWith (undefined :: Float) gToReal
    | dt == gdtFloat64  = writeWith (undefined :: Double) gToReal
    | dt == gdtCInt16   = writeWith (undefined :: Pair Int16) gToIntegralPair
    | dt == gdtCInt32   = writeWith (undefined :: Pair Int32) gToIntegralPair
    | dt == gdtCFloat32 = writeWith (undefined :: Pair Float) gToRealPair
    | dt == gdtCFloat64 = writeWith (undefined :: Pair Double) gToRealPair
    | otherwise         = error "gWrite: Invalid GType"
    where
      {-# INLINE writeWith #-}
      writeWith y f = inline writeByteArray# p# i# (inline f v `asTypeOf` y)
  {-# INLINE gWrite #-}

  gRead :: DataType -> Reader s a
  gRead !dt p# i# s#
    | dt == gdtByte     = readWith (undefined :: Word8) gFromIntegral
    | dt == gdtByte     = readWith (undefined :: Word8) gFromIntegral
    | dt == gdtUInt16   = readWith (undefined :: Word16) gFromIntegral
    | dt == gdtUInt32   = readWith (undefined :: Word32) gFromIntegral
    | dt == gdtInt16    = readWith (undefined :: Int16) gFromIntegral
    | dt == gdtInt32    = readWith (undefined :: Int32) gFromIntegral
    | dt == gdtFloat32  = readWith (undefined :: Float) gFromReal
    | dt == gdtFloat64  = readWith (undefined :: Double) gFromReal
    | dt == gdtCInt16   = readWith (undefined :: Pair Int16) gFromIntegralPair
    | dt == gdtCInt32   = readWith (undefined :: Pair Int32) gFromIntegralPair
    | dt == gdtCFloat32 = readWith (undefined :: Pair Float) gFromRealPair
    | dt == gdtCFloat64 = readWith (undefined :: Pair Double) gFromRealPair
    | otherwise         = error "gRead: Invalid GType"
    where
      {-# INLINE readWith #-}
      readWith y f =
        case inline readByteArray# p# i# s# of
          (# s1#, v #) -> (# s1#, inline f (v `asTypeOf` y) #)
  {-# INLINE gRead #-}

  gIndex :: DataType -> Indexer a
  gIndex !dt p# i#
    | dt == gdtByte     = indexWith (undefined :: Word8) gFromIntegral
    | dt == gdtUInt16   = indexWith (undefined :: Word16) gFromIntegral
    | dt == gdtUInt32   = indexWith (undefined :: Word32) gFromIntegral
    | dt == gdtInt16    = indexWith (undefined :: Int16) gFromIntegral
    | dt == gdtInt32    = indexWith (undefined :: Int32) gFromIntegral
    | dt == gdtFloat32  = indexWith (undefined :: Float) gFromReal
    | dt == gdtFloat64  = indexWith (undefined :: Double) gFromReal
    | dt == gdtCInt16   = indexWith (undefined :: Pair Int16) gFromIntegralPair
    | dt == gdtCInt32   = indexWith (undefined :: Pair Int32) gFromIntegralPair
    | dt == gdtCFloat32 = indexWith (undefined :: Pair Float) gFromRealPair
    | dt == gdtCFloat64 = indexWith (undefined :: Pair Double) gFromRealPair
    | otherwise         = error "gIndex: Invalid GType"
    where
      {-# INLINE indexWith #-}
      indexWith y f = inline f (inline indexByteArray# p# i# `asTypeOf` y)
  {-# INLINE gIndex #-}

  gToIntegral   :: Integral b => a -> b
  gFromIntegral :: Integral b => b -> a

  gToReal   :: RealFrac b => a -> b
  gFromReal :: RealFrac b => b -> a

  gToRealPair   :: RealFrac b => a -> Pair b
  gFromRealPair :: RealFrac b => Pair b -> a

  gToIntegralPair   :: Integral b => a -> Pair b
  gFromIntegralPair :: Integral b => Pair b -> a




convertGType :: forall a b. (GDALType a, GDALType b) => a -> b
convertGType a
  | adt == bdt = unsafeCoerce a
  | otherwise  = runST $ do
      MutableByteArray arr# <- newByteArray (sizeOfDataType bdt)
      primitive_ (gWrite bdt arr# 0# a)
      primitive  (gRead  bdt arr# 0#)
  where adt = dataType (Proxy :: Proxy a)
        bdt = dataType (Proxy :: Proxy b)
{-# INLINE convertGType #-}

instance GDALType Word8 where
  dataType _          = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType          #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Word16 where
  dataType          _ = gdtUInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Word32 where
  dataType          _ = gdtUInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Int8 where
  dataType          _ = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Int16 where
  dataType          _ = gdtInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Int32 where
  dataType          _ = gdtInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Float where
  dataType          _ = gdtFloat32
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = Pair (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Double where
  dataType          _ = gdtFloat64
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = Pair (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType CDouble where
  dataType          _ = gdtFloat64
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = Pair (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}


instance GDALType (Complex Int16) where
  dataType _          = gdtCInt16
  gToIntegral         = fromIntegral . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = fromIntegral . realPart
  gFromReal         v = truncate v :+ 0
  gToIntegralPair     = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap truncate
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}



instance GDALType (Complex Int32) where
  dataType          _ = gdtCInt32
  gToIntegral         = fromIntegral . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = fromIntegral . realPart
  gFromReal         v = truncate v :+ 0
  gToIntegralPair     = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap truncate
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType (Complex Float) where
  dataType          _ = gdtCFloat32
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair     = fmap truncate . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap realToFrac . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap realToFrac
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType (Complex Double) where
  dataType          _ = gdtCFloat64
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair     = fmap truncate . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap realToFrac . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap realToFrac
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
