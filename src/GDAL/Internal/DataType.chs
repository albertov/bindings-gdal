{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType (..)

  , convertGType
  , unsafeCopyWords

  , sizeOfDataType
  , alignOfDataType
) where

#include "gdal.h"
#include "bindings.h"

{#context lib = "gdal" prefix = "GDAL" #}

import Control.Monad (liftM, liftM2)

import Data.Int (Int8, Int16, Int32)
import Data.Complex (Complex(..), realPart)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)

import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with, copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import GDAL.Internal.Util (fromEnumC)



{#enum DataType {} omit (GDT_TypeCount) deriving (Eq, Show, Bounded) #}

sizeOfDataType :: DataType -> Int
sizeOfDataType dt =
  case dt of
    GDT_Byte     -> {#sizeof GByte   #}
    GDT_UInt16   -> {#sizeof GUInt16 #}
    GDT_UInt32   -> {#sizeof GUInt32 #}
    GDT_Int16    -> {#sizeof GInt16  #}
    GDT_Int32    -> {#sizeof GInt32  #}
    GDT_Float32  -> sizeOf (undefined :: CFloat)
    GDT_Float64  -> sizeOf (undefined :: CDouble)
    GDT_CInt16   -> {#sizeof GInt16  #} * 2
    GDT_CInt32   -> {#sizeof GInt32  #} * 2
    GDT_CFloat32 -> sizeOf (undefined :: CFloat) * 2
    GDT_CFloat64 -> sizeOf (undefined :: CDouble) * 2
    GDT_Unknown  -> error "sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}

alignOfDataType :: DataType -> Int
alignOfDataType dt =
  case dt of
    GDT_Byte     -> {#alignof GByte   #}
    GDT_UInt16   -> {#alignof GUInt16 #}
    GDT_UInt32   -> {#alignof GUInt32 #}
    GDT_Int16    -> {#alignof GInt16  #}
    GDT_Int32    -> {#alignof GInt32  #}
    GDT_Float32  -> alignment (undefined :: CFloat)
    GDT_Float64  -> sizeOf (undefined :: CDouble)
    GDT_CInt16   -> {#alignof GInt16  #}
    GDT_CInt32   -> {#alignof GInt32  #}
    GDT_CFloat32 -> alignment (undefined :: CFloat)
    GDT_CFloat64 -> alignment (undefined :: CDouble)
    GDT_Unknown  -> error "alignOfDataType: GDT_Unknown"
{-# INLINE alignOfDataType #-}

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class Eq a  => GDALType a where
  dataType :: Proxy a -> DataType

  gPeekElemOff :: DataType -> Ptr () -> Int -> IO a
  gPokeElemOff :: DataType -> Ptr () -> Int -> a -> IO ()

gUnsafeCopyWords
  :: forall dst src. (GDALType dst, GDALType src)
  => Ptr dst -> Ptr src -> Int -> IO ()
gUnsafeCopyWords dst src =
  unsafeCopyWords
    (castPtr dst)
    (dataType (Proxy :: Proxy dst))
    (castPtr src)
    (dataType (Proxy :: Proxy src))
{-# INLINE gUnsafeCopyWords #-}

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
  dataType _ = GDT_Byte
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CUChar where
  dataType _ = GDT_Byte
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Word16 where
  dataType _ = GDT_UInt16
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CUShort where
  dataType _ = GDT_UInt16
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Word32 where
  dataType _ = GDT_UInt32
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CUInt where
  dataType _ = GDT_UInt32
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int8 where
  dataType _ = GDT_Byte
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CSChar where
  dataType _ = GDT_Byte
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int16 where
  dataType _ = GDT_Int16
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CShort where
  dataType _ = GDT_Int16
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Int32 where
  dataType _ = GDT_Int32
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CInt where
  dataType _ = GDT_Int32
  gPeekElemOff = gPeekElemOffInt
  gPokeElemOff = gPokeElemOffInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Float where
  dataType _ = GDT_Float32
  gPeekElemOff = gPeekElemOffReal
  gPokeElemOff = gPokeElemOffReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType CFloat where
  dataType _ = GDT_Float32
  gPeekElemOff = gPeekElemOffReal
  gPokeElemOff = gPokeElemOffReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType Double where
  dataType _ = GDT_Float64
  gPeekElemOff = gPeekElemOffReal
  gPokeElemOff = gPokeElemOffReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}


instance GDALType CDouble where
  dataType _ = GDT_Float64
  gPeekElemOff = gPeekElemOffReal
  gPokeElemOff = gPokeElemOffReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}


#ifdef STORABLE_COMPLEX
instance GDALType (Complex Int16) where
  dataType _ = GDT_CInt16
  gPeekElemOff = gPeekElemOffCInt
  gPokeElemOff = gPokeElemOffCInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Int32) where
  dataType _ = GDT_CInt32
  gPeekElemOff = gPeekElemOffCInt
  gPokeElemOff = gPokeElemOffCInt
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Float) where
  dataType _ = GDT_CFloat32
  gPeekElemOff = gPeekElemOffCReal
  gPokeElemOff = gPokeElemOffCReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}

instance GDALType (Complex Double) where
  dataType _ = GDT_CFloat64
  gPeekElemOff = gPeekElemOffCReal
  gPokeElemOff = gPokeElemOffCReal
  {-# INLINE dataType #-}
  {-# INLINE gPeekElemOff #-}
  {-# INLINE gPokeElemOff #-}
#endif

gPeekElemOffInt :: Integral a => DataType -> Ptr b -> Int -> IO a
gPeekElemOffInt dt p =
  case dt of
    GDT_Byte     -> liftM fromIntegral              . peekWord8   p
    GDT_UInt16   -> liftM fromIntegral              . peekWord16  p
    GDT_UInt32   -> liftM fromIntegral              . peekWord32  p
    GDT_Int16    -> liftM fromIntegral              . peekInt16   p
    GDT_Int32    -> liftM fromIntegral              . peekInt32   p
    GDT_Float32  -> liftM truncate                  . peekReal32  p
    GDT_Float64  -> liftM truncate                  . peekReal64  p
    GDT_CInt16   -> liftM (fromIntegral . realPart) . peekCInt16  p
    GDT_CInt32   -> liftM (fromIntegral . realPart) . peekCInt32  p
    GDT_CFloat32 -> liftM (truncate     . realPart) . peekCReal32 p
    GDT_CFloat64 -> liftM (truncate     . realPart) . peekCReal64 p
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffInt #-}

gPokeElemOffInt :: Integral a => DataType -> Ptr b -> Int -> a -> IO ()
gPokeElemOffInt dt p i v =
  case dt of
    GDT_Byte     -> pokeWord8   p i (fromIntegral v)
    GDT_UInt16   -> pokeWord16  p i (fromIntegral v)
    GDT_UInt32   -> pokeWord32  p i (fromIntegral v)
    GDT_Int16    -> pokeWord16  p i (fromIntegral v)
    GDT_Int32    -> pokeWord32  p i (fromIntegral v)
    GDT_Float32  -> pokeReal32  p i (fromIntegral v)
    GDT_Float64  -> pokeReal64  p i (fromIntegral v)
    GDT_CInt16   -> pokeCInt16  p i (fromIntegral v :+ 0)
    GDT_CInt32   -> pokeCInt32  p i (fromIntegral v :+ 0)
    GDT_CFloat32 -> pokeCReal32 p i (fromIntegral v :+ 0)
    GDT_CFloat64 -> pokeCReal64 p i (fromIntegral v :+ 0)
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffInt #-}

gPeekElemOffReal :: RealFrac a => DataType -> Ptr b -> Int -> IO a
gPeekElemOffReal dt p =
  case dt of
    GDT_Byte     -> liftM realToFrac              . peekWord8   p
    GDT_UInt16   -> liftM realToFrac              . peekWord16  p
    GDT_UInt32   -> liftM realToFrac              . peekWord32  p
    GDT_Int16    -> liftM realToFrac              . peekInt16   p
    GDT_Int32    -> liftM realToFrac              . peekInt32   p
    GDT_Float32  -> liftM realToFrac              . peekReal32  p
    GDT_Float64  -> liftM realToFrac              . peekReal64  p
    GDT_CInt16   -> liftM (realToFrac . realPart) . peekCInt16  p
    GDT_CInt32   -> liftM (realToFrac . realPart) . peekCInt32  p
    GDT_CFloat32 -> liftM (realToFrac . realPart) . peekCReal32 p
    GDT_CFloat64 -> liftM (realToFrac . realPart) . peekCReal64 p
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffReal #-}

gPokeElemOffReal :: RealFrac a => DataType -> Ptr b -> Int -> a -> IO ()
gPokeElemOffReal dt p i v =
  case dt of
    GDT_Byte     -> pokeWord8   p i (truncate v)
    GDT_UInt16   -> pokeWord16  p i (truncate v)
    GDT_UInt32   -> pokeWord32  p i (truncate v)
    GDT_Int16    -> pokeWord16  p i (truncate v)
    GDT_Int32    -> pokeWord32  p i (truncate v)
    GDT_Float32  -> pokeReal32  p i (realToFrac v)
    GDT_Float64  -> pokeReal64  p i (realToFrac v)
    GDT_CInt16   -> pokeCInt16  p i (truncate v :+ 0)
    GDT_CInt32   -> pokeCInt32  p i (truncate v :+ 0)
    GDT_CFloat32 -> pokeCReal32 p i (realToFrac v :+ 0)
    GDT_CFloat64 -> pokeCReal64 p i (realToFrac v :+ 0)
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffReal #-}


gPeekElemOffCInt :: Integral a => DataType -> Ptr b -> Int -> IO (Complex a)
gPeekElemOffCInt dt p =
  case dt of
    GDT_Byte     -> liftM (woImag . fromIntegral) . peekWord8   p
    GDT_UInt16   -> liftM (woImag . fromIntegral) . peekWord16  p
    GDT_UInt32   -> liftM (woImag . fromIntegral) . peekWord32  p
    GDT_Int16    -> liftM (woImag . fromIntegral) . peekInt16   p
    GDT_Int32    -> liftM (woImag . fromIntegral) . peekInt32   p
    GDT_Float32  -> liftM (woImag . truncate    ) . peekReal32  p
    GDT_Float64  -> liftM (woImag . truncate    ) . peekReal64  p
    GDT_CInt16   -> liftM (cmap fromIntegral    ) . peekCInt16  p
    GDT_CInt32   -> liftM (cmap fromIntegral    ) . peekCInt32  p
    GDT_CFloat32 -> liftM (cmap truncate        ) . peekCReal32 p
    GDT_CFloat64 -> liftM (cmap truncate        ) . peekCReal64 p
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffCInt #-}

gPokeElemOffCInt
  :: Integral a => DataType -> Ptr b -> Int -> (Complex a) -> IO ()
gPokeElemOffCInt dt p i v =
  case dt of
    GDT_Byte     -> pokeWord8   p i (fromIntegral (realPart v))
    GDT_UInt16   -> pokeWord16  p i (fromIntegral (realPart v))
    GDT_UInt32   -> pokeWord32  p i (fromIntegral (realPart v))
    GDT_Int16    -> pokeWord16  p i (fromIntegral (realPart v))
    GDT_Int32    -> pokeWord32  p i (fromIntegral (realPart v))
    GDT_Float32  -> pokeReal32  p i (fromIntegral (realPart v))
    GDT_Float64  -> pokeReal64  p i (fromIntegral (realPart v))
    GDT_CInt16   -> pokeCInt16  p i (cmap fromIntegral v)
    GDT_CInt32   -> pokeCInt32  p i (cmap fromIntegral v)
    GDT_CFloat32 -> pokeCReal32 p i (cmap fromIntegral v)
    GDT_CFloat64 -> pokeCReal64 p i (cmap fromIntegral v)
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffCInt #-}


gPeekElemOffCReal :: RealFrac a => DataType -> Ptr b -> Int -> IO (Complex a)
gPeekElemOffCReal dt p =
  case dt of
    GDT_Byte     -> liftM (woImag . realToFrac) . peekWord8   p
    GDT_UInt16   -> liftM (woImag . realToFrac) . peekWord16  p
    GDT_UInt32   -> liftM (woImag . realToFrac) . peekWord32  p
    GDT_Int16    -> liftM (woImag . realToFrac) . peekInt16   p
    GDT_Int32    -> liftM (woImag . realToFrac) . peekInt32   p
    GDT_Float32  -> liftM (woImag . realToFrac) . peekReal32  p
    GDT_Float64  -> liftM (woImag . realToFrac) . peekReal64  p
    GDT_CInt16   -> liftM (cmap realToFrac    ) . peekCInt16  p
    GDT_CInt32   -> liftM (cmap realToFrac    ) . peekCInt32  p
    GDT_CFloat32 -> liftM (cmap realToFrac    ) . peekCReal32 p
    GDT_CFloat64 -> liftM (cmap realToFrac    ) . peekCReal64 p
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPeekElemOffCReal #-}

gPokeElemOffCReal
  :: RealFrac a => DataType -> Ptr b -> Int -> (Complex a) -> IO ()
gPokeElemOffCReal dt p i v =
  case dt of
    GDT_Byte     -> pokeWord8   p i (truncate (realPart v))
    GDT_UInt16   -> pokeWord16  p i (truncate (realPart v))
    GDT_UInt32   -> pokeWord32  p i (truncate (realPart v))
    GDT_Int16    -> pokeWord16  p i (truncate (realPart v))
    GDT_Int32    -> pokeWord32  p i (truncate (realPart v))
    GDT_Float32  -> pokeReal32  p i (realToFrac (realPart v))
    GDT_Float64  -> pokeReal64  p i (realToFrac (realPart v))
    GDT_CInt16   -> pokeCInt16  p i (cmap truncate v)
    GDT_CInt32   -> pokeCInt32  p i (cmap truncate v)
    GDT_CFloat32 -> pokeCReal32 p i (cmap realToFrac v)
    GDT_CFloat64 -> pokeCReal64 p i (cmap realToFrac v)
    GDT_Unknown  -> error "gPeekElemOff: GDT_Unknown"
{-# INLINE gPokeElemOffCReal #-}

woImag :: Num a => a -> Complex a
woImag v = v :+ 0
{-# INLINE woImag #-}

cmap :: (t -> a) -> Complex t -> Complex a
cmap f (a :+ b) = f a :+ f b
{-# INLINE cmap #-}

peekWord8 :: Ptr b -> Int -> IO Word8
peekWord8 = peekByteOff
{-# INLINE peekWord8 #-}

peekWord16 :: Ptr b -> Int -> IO Word16
peekWord16 p i = peekByteOff p (i*2)
{-# INLINE peekWord16 #-}

peekWord32 :: Ptr b -> Int -> IO Word32
peekWord32 p i = peekByteOff p (i*4)
{-# INLINE peekWord32 #-}

peekInt16 :: Ptr b -> Int -> IO Int16
peekInt16 p i = peekByteOff p (i*2)
{-# INLINE peekInt16 #-}

peekInt32 :: Ptr b -> Int -> IO Int32
peekInt32 p i = peekByteOff p (i*4)
{-# INLINE peekInt32 #-}


peekReal32 :: Ptr b -> Int -> IO Float
peekReal32 p i = peekByteOff p (i*4)
{-# INLINE peekReal32 #-}

peekReal64 :: Ptr b -> Int -> IO Double
peekReal64 p i = peekByteOff p (i*8)
{-# INLINE peekReal64 #-}

peekCInt16 :: Ptr b -> Int -> IO (Complex Int16)
peekCInt16 p i = liftM2 (:+) (peekInt16 p (i*2)) (peekInt16 p (i*2+1))
{-# INLINE peekCInt16 #-}

peekCInt32 :: Ptr b -> Int -> IO (Complex Int32)
peekCInt32 p i = liftM2 (:+) (peekInt32 p (i*2)) (peekInt32 p (i*2+1))
{-# INLINE peekCInt32 #-}

peekCReal32 :: Ptr b -> Int -> IO (Complex Float)
peekCReal32 p i = liftM2 (:+) (peekReal32 p (i*2)) (peekReal32 p (i*2+1))
{-# INLINE peekCReal32 #-}

peekCReal64 :: Ptr b -> Int -> IO (Complex Double)
peekCReal64 p i = liftM2 (:+) (peekReal64 p (i*2)) (peekReal64 p (i*2+1))
{-# INLINE peekCReal64 #-}



pokeWord8 :: Ptr b -> Int -> Word8 -> IO ()
pokeWord8 = pokeByteOff
{-# INLINE pokeWord8 #-}

pokeWord16 :: Ptr b -> Int -> Word16 -> IO ()
pokeWord16 p i = pokeByteOff p (i*2)
{-# INLINE pokeWord16 #-}

pokeWord32 :: Ptr b -> Int -> Word32 -> IO ()
pokeWord32 p i = pokeByteOff p (i*4)
{-# INLINE pokeWord32 #-}

pokeInt16 :: Ptr b -> Int -> Int16 -> IO ()
pokeInt16 p i = pokeByteOff p (i*2)
{-# INLINE pokeInt16 #-}

pokeInt32 :: Ptr b -> Int -> Int32 -> IO ()
pokeInt32 p i = pokeByteOff p (i*4)
{-# INLINE pokeInt32 #-}

pokeReal32 :: Ptr b -> Int -> Float -> IO ()
pokeReal32 p i = pokeByteOff p (i*4)
{-# INLINE pokeReal32 #-}

pokeReal64 :: Ptr b -> Int -> Double -> IO ()
pokeReal64 p i = pokeByteOff p (i*8)
{-# INLINE pokeReal64 #-}

pokeCInt16 :: Ptr b -> Int -> Complex Int16 -> IO ()
pokeCInt16 p i (a:+b) = pokeInt16 p (i*2) a >> pokeInt16 p (i*2+1) b
{-# INLINE pokeCInt16 #-}

pokeCInt32 :: Ptr b -> Int -> Complex Int32 -> IO ()
pokeCInt32 p i (a:+b) = pokeInt32 p (i*2) a >> pokeInt32 p (i*2+1) b
{-# INLINE pokeCInt32 #-}

pokeCReal32 :: Ptr b -> Int -> Complex Float -> IO ()
pokeCReal32 p i (a:+b) = pokeReal32 p (i*2) a >> pokeReal32 p (i*2+1) b
{-# INLINE pokeCReal32 #-}

pokeCReal64 :: Ptr b -> Int -> Complex Double -> IO ()
pokeCReal64 p i (a:+b) = pokeReal64 p (i*2) a >> pokeReal64 p (i*2+1) b
{-# INLINE pokeCReal64 #-}
