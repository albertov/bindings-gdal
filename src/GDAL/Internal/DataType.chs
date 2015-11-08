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

import Control.Arrow ((***))
import Control.Monad (liftM, liftM2)
import Control.Monad.Primitive

import Data.Primitive.Addr
import Data.Primitive.Types (Prim)

import Data.Int (Int8, Int16, Int32)
import Data.Complex (Complex(..), realPart, imagPart)
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
  toEnum = DataType
  {-# INLINE toEnum #-}

  fromEnum (DataType a) = a
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

type Pair a = (a, a)
------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class Eq a  => GDALType a where
  dataType :: Proxy a -> DataType

  gPokeElemOff :: DataType -> Ptr () -> Int -> a -> IO ()
  gPokeElemOff !dt !(Ptr a) !i = gWriteOffAddr dt (Addr a) i
  {-# INLINE gPokeElemOff #-}

  gPeekElemOff :: DataType -> Ptr () -> Int -> IO a
  gPeekElemOff !dt !(Ptr a) = gReadOffAddr dt (Addr a)
  {-# INLINE gPeekElemOff #-}

  gWriteOffAddr :: PrimMonad m => DataType -> Addr -> Int -> a -> m ()
  gWriteOffAddr !dt !p !i v =
    case dt of
      _ | dt == gdtByte     -> writeOffAddr p i (gToIntegral v :: Word8)
      _ | dt == gdtUInt16   -> writeOffAddr p i (gToIntegral v :: Word16)
      _ | dt == gdtUInt32   -> writeOffAddr p i (gToIntegral v :: Word32)
      _ | dt == gdtInt16    -> writeOffAddr p i (gToIntegral v :: Int16)
      _ | dt == gdtInt32    -> writeOffAddr p i (gToIntegral v :: Int32)
      _ | dt == gdtFloat32  -> writeOffAddr p i (gToReal     v :: Float)
      _ | dt == gdtFloat64  -> writeOffAddr p i (gToReal     v :: Double)
      _ | dt == gdtCInt16   ->
            writeOffAddrPair p i (gToIntegralPair v :: Pair Int16)
      _ | dt == gdtCInt32   ->
            writeOffAddrPair p i (gToIntegralPair v :: Pair Int32)
      _ | dt == gdtCFloat32 ->
            writeOffAddrPair p i (gToRealPair     v :: Pair Float)
      _ | dt == gdtCFloat64 ->
            writeOffAddrPair p i (gToRealPair     v :: Pair Double)
      _ -> error "gWriteOffAddr: Invalid GType"
  {-# INLINE gWriteOffAddr #-}

  gReadOffAddr :: PrimMonad m => DataType -> Addr -> Int -> m a
  gReadOffAddr !dt !p !i =
    case dt of
      _ | dt == gdtByte     ->
            liftM (gFromIntegral :: Word8 -> a) (readOffAddr p i)
      _ | dt == gdtUInt16   ->
            liftM (gFromIntegral :: Word16 -> a) (readOffAddr p i)
      _ | dt == gdtUInt32   ->
            liftM (gFromIntegral :: Word32 -> a) (readOffAddr p i)
      _ | dt == gdtInt16    ->
            liftM (gFromIntegral :: Int16 -> a) (readOffAddr p i)
      _ | dt == gdtInt32    ->
            liftM (gFromIntegral :: Int32 -> a) (readOffAddr p i)
      _ | dt == gdtFloat32  ->
            liftM (gFromReal :: Float -> a) (readOffAddr p i)
      _ | dt == gdtFloat64  ->
            liftM (gFromReal :: Double -> a) (readOffAddr p i)
      _ | dt == gdtCInt16   ->
            liftM (gFromIntegralPair :: Pair Int16 -> a) (readOffAddrPair p i)
      _ | dt == gdtCInt32   ->
            liftM (gFromIntegralPair :: Pair Int32 -> a) (readOffAddrPair p i)
      _ | dt == gdtCFloat32 ->
            liftM (gFromRealPair :: Pair Float -> a) (readOffAddrPair p i)
      _ | dt == gdtCFloat64 ->
            liftM (gFromRealPair :: Pair Double -> a) (readOffAddrPair p i)
      _ -> error "gReadOffAddr: Invalid GType"
  {-# INLINE gReadOffAddr #-}

  gIndexOffAddr :: DataType -> Addr -> Int -> a
  gIndexOffAddr !dt !p =
    case dt of
      _ | dt == gdtByte     -> (gFromIntegral :: Word8  -> a) . indexOffAddr p
      _ | dt == gdtUInt16   -> (gFromIntegral :: Word16 -> a) . indexOffAddr p
      _ | dt == gdtUInt32   -> (gFromIntegral :: Word32 -> a) . indexOffAddr p
      _ | dt == gdtInt16    -> (gFromIntegral :: Int16  -> a) . indexOffAddr p
      _ | dt == gdtInt32    -> (gFromIntegral :: Int32  -> a) . indexOffAddr p
      _ | dt == gdtFloat32  -> (gFromReal     :: Float  -> a) . indexOffAddr p
      _ | dt == gdtFloat64  -> (gFromReal     :: Double -> a) . indexOffAddr p
      _ | dt == gdtCInt16   ->
            (gFromIntegralPair :: Pair Int16  -> a) . indexOffAddrPair p
      _ | dt == gdtCInt32   ->
            (gFromIntegralPair :: Pair Int32  -> a) . indexOffAddrPair p
      _ | dt == gdtCFloat32 ->
            (gFromRealPair     :: Pair Float  -> a) . indexOffAddrPair p
      _ | dt == gdtCFloat64 ->
            (gFromRealPair     :: Pair Double -> a) . indexOffAddrPair p
      _ -> error "gIndexOffAddr: Invalid GType"
  {-# INLINE gIndexOffAddr #-}

  gToIntegral   :: Integral b => a -> b
  gFromIntegral :: Integral b => b -> a

  gToReal   :: RealFrac b => a -> b
  gFromReal :: RealFrac b => b -> a

  gToRealPair   :: RealFrac b => a -> (b, b)
  gFromRealPair :: RealFrac b => (b, b) -> a

  gToIntegralPair   :: Integral b => a -> (b, b)
  gFromIntegralPair :: Integral b => (b, b) -> a

{-# RULES "gReadOffAddr/int16" gReadOffAddr gdtInt16 = (readOffAddr :: Addr -> Int -> IO Int16) #-}


readOffAddrPair :: (Prim a, PrimMonad m) => Addr -> Int -> m (Pair a)
readOffAddrPair a i = liftM2 (,) (readOffAddr a (i*2    ))
                                 (readOffAddr a (i*2 + 1))
{-# INLINE readOffAddrPair #-}

indexOffAddrPair :: Prim a => Addr -> Int -> Pair a
indexOffAddrPair a i = let v = indexOffAddr a (i*2    )
                           w = indexOffAddr a (i*2 + 1)
                       in (v, w)
{-# INLINE indexOffAddrPair #-}

writeOffAddrPair :: (Prim a, PrimMonad m) => Addr -> Int -> (Pair a) -> m ()
writeOffAddrPair a i (v,w) = do writeOffAddr a (i*2    ) v
                                writeOffAddr a (i*2 + 1) w
{-# INLINE writeOffAddrPair #-}

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
  dataType _      = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}

instance GDALType Word16 where
  dataType _ = gdtUInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
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
  dataType _ = gdtUInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
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
  dataType _ = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
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
  dataType _ = gdtInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
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
  dataType _ = gdtInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (fromIntegral v, 0)
  gFromRealPair       = truncate . fst
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
  dataType _ = gdtFloat32
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst
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
  dataType _ = gdtFloat64
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst
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
  dataType _ = gdtFloat64
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst
  gToRealPair       v = (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst
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
  gToIntegralPair   v = (fromIntegral (realPart v), fromIntegral (imagPart v))
  gFromIntegralPair   = uncurry (:+) . (fromIntegral *** fromIntegral)
  gToRealPair       v = (fromIntegral (realPart v), fromIntegral (imagPart v))
  gFromRealPair       = uncurry (:+) . (truncate *** truncate)
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
  dataType _ = gdtCInt32
  gToIntegral         = fromIntegral . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = fromIntegral . realPart
  gFromReal         v = truncate v :+ 0
  gToIntegralPair   v = (fromIntegral (realPart v), fromIntegral (imagPart v))
  gFromIntegralPair   = uncurry (:+) . (fromIntegral *** fromIntegral)
  gToRealPair       v = (fromIntegral (realPart v), fromIntegral (imagPart v))
  gFromRealPair       = uncurry (:+) . (truncate *** truncate)
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
  dataType _ = gdtCFloat32
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair   v = (truncate (realPart v), truncate (imagPart v))
  gFromIntegralPair   = uncurry (:+) . (fromIntegral *** fromIntegral)
  gToRealPair       v = (realToFrac (realPart v), realToFrac (imagPart v))
  gFromRealPair       = uncurry (:+) . (realToFrac *** realToFrac)
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
  dataType _ = gdtCFloat64
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair   v = (truncate (realPart v), truncate (imagPart v))
  gFromIntegralPair   = uncurry (:+) . (fromIntegral *** fromIntegral)
  gToRealPair       v = (realToFrac (realPart v), realToFrac (imagPart v))
  gFromRealPair       = uncurry (:+) . (realToFrac *** realToFrac)
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
