{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType

  , convertGType
  , unsafeCopyWords

  , sizeOfDataType
  , alignOfDataType

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

#include "gdal.h"

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Monad (liftM, liftM2)

import Data.Primitive.Addr

import Data.Int (Int8, Int16, Int32)
import Data.Complex (Complex(..), realPart)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)

import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (sizeOf, alignment)

import GHC.Ptr (Ptr(..))

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import GDAL.Internal.Util (fromEnumC)

newtype DataType = DataType Int
  deriving Eq

instance Show DataType where
  show (DataType d) = unsafePerformIO $
    {#call unsafe GetDataTypeName as ^#} (fromIntegral d) >>= peekCString 

#c
#define GDT_Unknown   0
#define GDT_Byte      1
#define GDT_UInt16    2
#define GDT_Int16     3
#define GDT_UInt32    4
#define GDT_Int32     5
#define GDT_Float32   6
#define GDT_Float64   7
#define GDT_CInt16    8
#define GDT_CInt32    9
#define GDT_CFloat32  10
#define GDT_CFloat64  11
#endc

instance Enum DataType where
  toEnum {#const GDT_Byte     #} = DataType {#const GDT_Byte#}
  toEnum {#const GDT_UInt16   #} = DataType {#const GDT_UInt16#}
  toEnum {#const GDT_UInt32   #} = DataType {#const GDT_UInt32#}
  toEnum {#const GDT_Int16    #} = DataType {#const GDT_Int16#}
  toEnum {#const GDT_Int32    #} = DataType {#const GDT_Int32#}
  toEnum {#const GDT_Float32  #} = DataType {#const GDT_Float32#}
  toEnum {#const GDT_Float64  #} = DataType {#const GDT_Float64#}
  toEnum {#const GDT_CInt16   #} = DataType {#const GDT_CInt16#}
  toEnum {#const GDT_CInt32   #} = DataType {#const GDT_CInt32#}
  toEnum {#const GDT_CFloat32 #} = DataType {#const GDT_CFloat32#}
  toEnum {#const GDT_CFloat64 #} = DataType {#const GDT_CFloat64#}
  toEnum {#const GDT_Unknown  #} = DataType {#const GDT_Unknown#}
  toEnum _ = error "DataType: Invalid toEnum"
  {-# INLINE toEnum #-}

  fromEnum (DataType {#const GDT_Byte     #}) = {#const GDT_Byte#}
  fromEnum (DataType {#const GDT_UInt16   #}) = {#const GDT_UInt16#}
  fromEnum (DataType {#const GDT_UInt32   #}) = {#const GDT_UInt32#}
  fromEnum (DataType {#const GDT_Int16    #}) = {#const GDT_Int16#}
  fromEnum (DataType {#const GDT_Int32    #}) = {#const GDT_Int32#}
  fromEnum (DataType {#const GDT_Float32  #}) = {#const GDT_Float32#}
  fromEnum (DataType {#const GDT_Float64  #}) = {#const GDT_Float64#}
  fromEnum (DataType {#const GDT_CInt16   #}) = {#const GDT_CInt16#}
  fromEnum (DataType {#const GDT_CInt32   #}) = {#const GDT_CInt32#}
  fromEnum (DataType {#const GDT_CFloat32 #}) = {#const GDT_CFloat32#}
  fromEnum (DataType {#const GDT_CFloat64 #}) = {#const GDT_CFloat64#}
  fromEnum (DataType {#const GDT_Unknown  #}) = {#const GDT_Unknown#}
  fromEnum _  = error "DataType: Invalid fromEnum"
  {-# INLINE fromEnum #-}

instance Bounded DataType where
  minBound = gdtByte
  maxBound = gdtCFloat64

{-# INLINE gdtByte #-}
gdtByte :: DataType
gdtByte = DataType {#const GDT_Byte#}
{-# INLINE gdtUInt16 #-}
gdtUInt16 :: DataType
gdtUInt16 = DataType {#const GDT_UInt16#}
{-# INLINE gdtUInt32 #-}
gdtUInt32 :: DataType
gdtUInt32 = DataType {#const GDT_UInt32#}
{-# INLINE gdtInt16 #-}
gdtInt16 :: DataType
gdtInt16 = DataType {#const GDT_Int16#}
{-# INLINE gdtInt32 #-}
gdtInt32 :: DataType
gdtInt32 = DataType {#const GDT_Int32#}
{-# INLINE gdtFloat32 #-}
gdtFloat32 :: DataType
gdtFloat32 = DataType {#const GDT_Float32#}
{-# INLINE gdtFloat64 #-}
gdtFloat64 :: DataType
gdtFloat64 = DataType {#const GDT_Float64#}
{-# INLINE gdtCInt16 #-}
gdtCInt16 :: DataType
gdtCInt16 = DataType {#const GDT_CInt16#}
{-# INLINE gdtCInt32 #-}
gdtCInt32 :: DataType
gdtCInt32 = DataType {#const GDT_CInt32#}
{-# INLINE gdtCFloat32 #-}
gdtCFloat32 :: DataType
gdtCFloat32 = DataType {#const GDT_CFloat32#}
{-# INLINE gdtCFloat64 #-}
gdtCFloat64 :: DataType
gdtCFloat64 = DataType {#const GDT_CFloat64#}
{-# INLINE gdtUnknown #-}
gdtUnknown :: DataType
gdtUnknown = DataType {#const GDT_Unknown#}

sizeOfDataType :: DataType -> Int
sizeOfDataType dt =
  case dt of
    _ | dt == gdtByte     -> {#sizeof GByte   #}
    _ | dt == gdtUInt16   -> {#sizeof GUInt16 #}
    _ | dt == gdtUInt32   -> {#sizeof GUInt32 #}
    _ | dt == gdtInt16    -> {#sizeof GInt16  #}
    _ | dt == gdtInt32    -> {#sizeof GInt32  #}
    _ | dt == gdtFloat32  -> sizeOf (undefined :: CFloat)
    _ | dt == gdtFloat64  -> sizeOf (undefined :: CDouble)
    _ | dt == gdtCInt16   -> {#sizeof GInt16  #} * 2
    _ | dt == gdtCInt32   -> {#sizeof GInt32  #} * 2
    _ | dt == gdtCFloat32 -> sizeOf (undefined :: CFloat) * 2
    _ | dt == gdtCFloat64 -> sizeOf (undefined :: CDouble) * 2
    _                     -> error "sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}

alignOfDataType :: DataType -> Int
alignOfDataType dt =
  case dt of
    _ | dt == gdtByte     -> {#alignof GByte   #}
    _ | dt == gdtUInt16   -> {#alignof GUInt16 #}
    _ | dt == gdtUInt32   -> {#alignof GUInt32 #}
    _ | dt == gdtInt16    -> {#alignof GInt16  #}
    _ | dt == gdtInt32    -> {#alignof GInt32  #}
    _ | dt == gdtFloat32  -> alignment (undefined :: CFloat)
    _ | dt == gdtFloat64  -> sizeOf (undefined :: CDouble)
    _ | dt == gdtCInt16   -> {#alignof GInt16  #}
    _ | dt == gdtCInt32   -> {#alignof GInt32  #}
    _ | dt == gdtCFloat32 -> alignment (undefined :: CFloat)
    _ | dt == gdtCFloat64 -> alignment (undefined :: CDouble)
    _                     -> error "alignOfDataType: GDT_Unknown"
{-# INLINE alignOfDataType #-}

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class Eq a  => GDALType a where
  dataType :: Proxy a -> DataType

  gPeekElemOff :: DataType -> Ptr () -> Int -> IO a
  gPokeElemOff :: DataType -> Ptr () -> Int -> a -> IO ()

unsafeCopyWords
  :: Ptr () -> DataType -> Ptr () -> DataType -> Int -> IO ()
unsafeCopyWords dst dDst src dSrc count
  | dDst == dSrc = copyBytes dst src (count * sizeOfDataType dDst)
  | otherwise =
    {#call unsafe GDALCopyWords as ^#}
      src (fromEnumC dSrc) (fromIntegral (sizeOfDataType dSrc))
      dst (fromEnumC dDst) (fromIntegral (sizeOfDataType dDst))
      (fromIntegral count)
{-# INLINE unsafeCopyWords #-}

convertGType :: forall a b. (GDALType a, GDALType b) => a -> b
convertGType a
  | adt == bdt = unsafeCoerce a
  | otherwise  = unsafePerformIO $
    allocaBytes (sizeOfDataType bdt) $ \bPtr -> do
      gPokeElemOff bdt bPtr 0 a
      gPeekElemOff bdt bPtr 0
  where adt = dataType (Proxy :: Proxy a) 
        bdt = dataType (Proxy :: Proxy b) 
{-# INLINE convertGType #-}

instance GDALType Word8 where
  dataType _ = gdtByte
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Word16 where
  dataType _ = gdtUInt16
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Word32 where
  dataType _ = gdtUInt32
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int8 where
  dataType _ = gdtByte
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int16 where
  dataType _ = gdtInt16
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int32 where
  dataType _ = gdtInt32
  gPeekElemOff !dt (Ptr p) = gPeekElemOffInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Float where
  dataType _ = gdtFloat32
  gPeekElemOff !dt (Ptr p) = gPeekElemOffReal dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffReal dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Double where
  dataType _ = gdtFloat64
  gPeekElemOff !dt (Ptr p) = gPeekElemOffReal dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffReal dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CDouble where
  dataType _ = gdtFloat64
  gPeekElemOff !dt (Ptr p) = gPeekElemOffReal dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffReal dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}


instance GDALType (Complex Int16) where
  dataType _ = gdtCInt16
  gPeekElemOff !dt (Ptr p) = gPeekElemOffCInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffCInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Int32) where
  dataType _ = gdtCInt32
  gPeekElemOff !dt (Ptr p) = gPeekElemOffCInt dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffCInt dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Float) where
  dataType _ = gdtCFloat32
  gPeekElemOff !dt (Ptr p) = gPeekElemOffCReal dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffCReal dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Double) where
  dataType _ = gdtCFloat64
  gPeekElemOff !dt (Ptr p) = gPeekElemOffCReal dt (Addr p)
  gPokeElemOff !dt (Ptr p) = gPokeElemOffCReal dt (Addr p)
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

gPeekElemOffInt :: Integral a => DataType -> Addr -> Int -> IO a
gPeekElemOffInt !dt p =
  case dt of
    _ | dt == gdtByte     -> liftM fromIntegral              . peekWord8   p
    _ | dt == gdtUInt16   -> liftM fromIntegral              . peekWord16  p
    _ | dt == gdtUInt32   -> liftM fromIntegral              . peekWord32  p
    _ | dt == gdtInt16    -> liftM fromIntegral              . peekInt16   p
    _ | dt == gdtInt32    -> liftM fromIntegral              . peekInt32   p
    _ | dt == gdtFloat32  -> liftM truncate                  . peekReal32  p
    _ | dt == gdtFloat64  -> liftM truncate                  . peekReal64  p
    _ | dt == gdtCInt16   -> liftM (fromIntegral . realPart) . peekCInt16  p
    _ | dt == gdtCInt32   -> liftM (fromIntegral . realPart) . peekCInt32  p
    _ | dt == gdtCFloat32 -> liftM (truncate     . realPart) . peekCReal32 p
    _ | dt == gdtCFloat64 -> liftM (truncate     . realPart) . peekCReal64 p
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffInt #-}

gPokeElemOffInt :: Integral a => DataType -> Addr -> Int-> a -> IO ()
gPokeElemOffInt !dt p i v =
  case dt of
    _ | dt == gdtByte     -> pokeWord8   p i (fromIntegral v)
    _ | dt == gdtUInt16   -> pokeWord16  p i (fromIntegral v)
    _ | dt == gdtUInt32   -> pokeWord32  p i (fromIntegral v)
    _ | dt == gdtInt16    -> pokeInt16   p i (fromIntegral v)
    _ | dt == gdtInt32    -> pokeInt32   p i (fromIntegral v)
    _ | dt == gdtFloat32  -> pokeReal32  p i (fromIntegral v)
    _ | dt == gdtFloat64  -> pokeReal64  p i (fromIntegral v)
    _ | dt == gdtCInt16   -> pokeCInt16  p i (fromIntegral v :+ 0)
    _ | dt == gdtCInt32   -> pokeCInt32  p i (fromIntegral v :+ 0)
    _ | dt == gdtCFloat32 -> pokeCReal32 p i (fromIntegral v :+ 0)
    _ | dt == gdtCFloat64 -> pokeCReal64 p i (fromIntegral v :+ 0)
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffInt #-}

gPeekElemOffReal :: RealFrac a => DataType -> Addr -> Int-> IO a
gPeekElemOffReal !dt p =
  case dt of
    _ | dt == gdtByte     -> liftM fromIntegral              . peekWord8   p
    _ | dt == gdtUInt16   -> liftM fromIntegral              . peekWord16  p
    _ | dt == gdtUInt32   -> liftM fromIntegral              . peekWord32  p
    _ | dt == gdtInt16    -> liftM fromIntegral              . peekInt16   p
    _ | dt == gdtInt32    -> liftM fromIntegral              . peekInt32   p
    _ | dt == gdtFloat32  -> liftM realToFrac                . peekReal32  p
    _ | dt == gdtFloat64  -> liftM realToFrac                . peekReal64  p
    _ | dt == gdtCInt16   -> liftM (fromIntegral . realPart) . peekCInt16  p
    _ | dt == gdtCInt32   -> liftM (fromIntegral . realPart) . peekCInt32  p
    _ | dt == gdtCFloat32 -> liftM (realToFrac . realPart)   . peekCReal32 p
    _ | dt == gdtCFloat64 -> liftM (realToFrac . realPart)   . peekCReal64 p
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffReal #-}

gPokeElemOffReal :: RealFrac a => DataType -> Addr -> Int-> a -> IO ()
gPokeElemOffReal !dt p i v =
  case dt of
    _ | dt == gdtByte     -> pokeWord8   p i (truncate v)
    _ | dt == gdtUInt16   -> pokeWord16  p i (truncate v)
    _ | dt == gdtUInt32   -> pokeWord32  p i (truncate v)
    _ | dt == gdtInt16    -> pokeInt16   p i (truncate v)
    _ | dt == gdtInt32    -> pokeInt32   p i (truncate v)
    _ | dt == gdtFloat32  -> pokeReal32  p i (realToFrac v)
    _ | dt == gdtFloat64  -> pokeReal64  p i (realToFrac v)
    _ | dt == gdtCInt16   -> pokeCInt16  p i (truncate v :+ 0)
    _ | dt == gdtCInt32   -> pokeCInt32  p i (truncate v :+ 0)
    _ | dt == gdtCFloat32 -> pokeCReal32 p i (realToFrac v :+ 0)
    _ | dt == gdtCFloat64 -> pokeCReal64 p i (realToFrac v :+ 0)
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffReal #-}


gPeekElemOffCInt :: Integral a => DataType -> Addr -> Int-> IO (Complex a)
gPeekElemOffCInt !dt p =
  case dt of
    _ | dt == gdtByte     -> liftM (woImag . fromIntegral) . peekWord8   p
    _ | dt == gdtUInt16   -> liftM (woImag . fromIntegral) . peekWord16  p
    _ | dt == gdtUInt32   -> liftM (woImag . fromIntegral) . peekWord32  p
    _ | dt == gdtInt16    -> liftM (woImag . fromIntegral) . peekInt16   p
    _ | dt == gdtInt32    -> liftM (woImag . fromIntegral) . peekInt32   p
    _ | dt == gdtFloat32  -> liftM (woImag . truncate    ) . peekReal32  p
    _ | dt == gdtFloat64  -> liftM (woImag . truncate    ) . peekReal64  p
    _ | dt == gdtCInt16   -> liftM (cmap fromIntegral    ) . peekCInt16  p
    _ | dt == gdtCInt32   -> liftM (cmap fromIntegral    ) . peekCInt32  p
    _ | dt == gdtCFloat32 -> liftM (cmap truncate        ) . peekCReal32 p
    _ | dt == gdtCFloat64 -> liftM (cmap truncate        ) . peekCReal64 p
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffCInt #-}

gPokeElemOffCInt
  :: Integral a => DataType -> Addr -> Int-> (Complex a) -> IO ()
gPokeElemOffCInt !dt p i v =
  case dt of
    _ | dt == gdtByte     -> pokeWord8   p i (fromIntegral (realPart v))
    _ | dt == gdtUInt16   -> pokeWord16  p i (fromIntegral (realPart v))
    _ | dt == gdtUInt32   -> pokeWord32  p i (fromIntegral (realPart v))
    _ | dt == gdtInt16    -> pokeInt16   p i (fromIntegral (realPart v))
    _ | dt == gdtInt32    -> pokeInt32   p i (fromIntegral (realPart v))
    _ | dt == gdtFloat32  -> pokeReal32  p i (fromIntegral (realPart v))
    _ | dt == gdtFloat64  -> pokeReal64  p i (fromIntegral (realPart v))
    _ | dt == gdtCInt16   -> pokeCInt16  p i (cmap fromIntegral v)
    _ | dt == gdtCInt32   -> pokeCInt32  p i (cmap fromIntegral v)
    _ | dt == gdtCFloat32 -> pokeCReal32 p i (cmap fromIntegral v)
    _ | dt == gdtCFloat64 -> pokeCReal64 p i (cmap fromIntegral v)
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffCInt #-}


gPeekElemOffCReal :: RealFrac a => DataType -> Addr -> Int-> IO (Complex a)
gPeekElemOffCReal !dt p =
  case dt of
    _ | dt == gdtByte     -> liftM (woImag . fromIntegral) . peekWord8   p
    _ | dt == gdtUInt16   -> liftM (woImag . fromIntegral) . peekWord16  p
    _ | dt == gdtUInt32   -> liftM (woImag . fromIntegral) . peekWord32  p
    _ | dt == gdtInt16    -> liftM (woImag . fromIntegral) . peekInt16   p
    _ | dt == gdtInt32    -> liftM (woImag . fromIntegral) . peekInt32   p
    _ | dt == gdtFloat32  -> liftM (woImag . realToFrac  ) . peekReal32  p
    _ | dt == gdtFloat64  -> liftM (woImag . realToFrac  ) . peekReal64  p
    _ | dt == gdtCInt16   -> liftM (cmap fromIntegral    ) . peekCInt16  p
    _ | dt == gdtCInt32   -> liftM (cmap fromIntegral    ) . peekCInt32  p
    _ | dt == gdtCFloat32 -> liftM (cmap realToFrac      ) . peekCReal32 p
    _ | dt == gdtCFloat64 -> liftM (cmap realToFrac      ) . peekCReal64 p
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffCReal #-}

gPokeElemOffCReal
  :: RealFrac a => DataType -> Addr -> Int-> (Complex a) -> IO ()
gPokeElemOffCReal !dt p i v =
  case dt of
    _ | dt == gdtByte     -> pokeWord8   p i (truncate (realPart v))
    _ | dt == gdtUInt16   -> pokeWord16  p i (truncate (realPart v))
    _ | dt == gdtUInt32   -> pokeWord32  p i (truncate (realPart v))
    _ | dt == gdtInt16    -> pokeInt16   p i (truncate (realPart v))
    _ | dt == gdtInt32    -> pokeInt32   p i (truncate (realPart v))
    _ | dt == gdtFloat32  -> pokeReal32  p i (realToFrac (realPart v))
    _ | dt == gdtFloat64  -> pokeReal64  p i (realToFrac (realPart v))
    _ | dt == gdtCInt16   -> pokeCInt16  p i (cmap truncate v)
    _ | dt == gdtCInt32   -> pokeCInt32  p i (cmap truncate v)
    _ | dt == gdtCFloat32 -> pokeCReal32 p i (cmap realToFrac v)
    _ | dt == gdtCFloat64 -> pokeCReal64 p i (cmap realToFrac v)
    _                     -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffCReal #-}

woImag :: Num a => a -> Complex a
woImag v = v :+ 0
{-# INLINE woImag #-}

cmap :: (t -> a) -> Complex t -> Complex a
cmap f (a :+ b) = f a :+ f b
{-# INLINE cmap #-}

peekWord8 :: Addr -> Int-> IO Word8
peekWord8 = readOffAddr
{-# INLINE peekWord8 #-}

peekWord16 :: Addr -> Int-> IO Word16
peekWord16 = readOffAddr
{-# INLINE peekWord16 #-}

peekWord32 :: Addr -> Int-> IO Word32
peekWord32 = readOffAddr
{-# INLINE peekWord32 #-}

peekInt16 :: Addr -> Int-> IO Int16
peekInt16 = readOffAddr
{-# INLINE peekInt16 #-}

peekInt32 :: Addr -> Int-> IO Int32
peekInt32 = readOffAddr
{-# INLINE peekInt32 #-}


peekReal32 :: Addr -> Int-> IO Float
peekReal32 = readOffAddr
{-# INLINE peekReal32 #-}

peekReal64 :: Addr -> Int-> IO Double
peekReal64 = readOffAddr
{-# INLINE peekReal64 #-}

peekCInt16 :: Addr -> Int-> IO (Complex Int16)
peekCInt16 p i = liftM2 (:+) (peekInt16 p (i*2)) (peekInt16 p (i*2+1))
{-# INLINE peekCInt16 #-}

peekCInt32 :: Addr -> Int-> IO (Complex Int32)
peekCInt32 p i = liftM2 (:+) (peekInt32 p (i*2)) (peekInt32 p (i*2+1))
{-# INLINE peekCInt32 #-}

peekCReal32 :: Addr -> Int-> IO (Complex Float)
peekCReal32 p i = liftM2 (:+) (peekReal32 p (i*2)) (peekReal32 p (i*2+1))
{-# INLINE peekCReal32 #-}

peekCReal64 :: Addr -> Int-> IO (Complex Double)
peekCReal64 p i = liftM2 (:+) (peekReal64 p (i*2)) (peekReal64 p (i*2+1))
{-# INLINE peekCReal64 #-}



pokeWord8 :: Addr -> Int-> Word8 -> IO ()
pokeWord8 = writeOffAddr
{-# INLINE pokeWord8 #-}

pokeWord16 :: Addr -> Int-> Word16 -> IO ()
pokeWord16 = writeOffAddr
{-# INLINE pokeWord16 #-}

pokeWord32 :: Addr -> Int-> Word32 -> IO ()
pokeWord32 = writeOffAddr
{-# INLINE pokeWord32 #-}

pokeInt16 :: Addr -> Int-> Int16 -> IO ()
pokeInt16 = writeOffAddr
{-# INLINE pokeInt16 #-}

pokeInt32 :: Addr -> Int-> Int32 -> IO ()
pokeInt32 = writeOffAddr
{-# INLINE pokeInt32 #-}

pokeReal32 :: Addr -> Int-> Float -> IO ()
pokeReal32  = writeOffAddr
{-# INLINE pokeReal32 #-}

pokeReal64 :: Addr -> Int-> Double -> IO ()
pokeReal64 = writeOffAddr
{-# INLINE pokeReal64 #-}

pokeCInt16 :: Addr -> Int-> Complex Int16 -> IO ()
pokeCInt16 p i (a:+b) = pokeInt16 p (i*2) a >> pokeInt16 p (i*2+1) b
{-# INLINE pokeCInt16 #-}

pokeCInt32 :: Addr -> Int-> Complex Int32 -> IO ()
pokeCInt32 p i (a:+b) = pokeInt32 p (i*2) a >> pokeInt32 p (i*2+1) b
{-# INLINE pokeCInt32 #-}

pokeCReal32 :: Addr -> Int-> Complex Float -> IO ()
pokeCReal32 p i (a:+b) = pokeReal32 p (i*2) a >> pokeReal32 p (i*2+1) b
{-# INLINE pokeCReal32 #-}

pokeCReal64 :: Addr -> Int-> Complex Double -> IO ()
pokeCReal64 p i (a:+b) = pokeReal64 p (i*2) a >> pokeReal64 p (i*2+1) b
{-# INLINE pokeCReal64 #-}
