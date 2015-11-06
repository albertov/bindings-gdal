{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.DataType (
    GDALType
  , DataType (..)
  , SomeGDALType (..)

  , convertGType
  , convertGTypeVector
  , unsafeConvertGTypeMVector

  , dataType
  , someGDALType
) where

#include "gdal.h"
#include "bindings.h"

{#context lib = "gdal" prefix = "GDAL" #}

import Data.Int (Int8, Int16, Int32)
import Data.Complex (Complex(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import GDAL.Internal.Util (fromEnumC)



{#enum DataType {} omit (GDT_TypeCount) deriving (Eq, Show, Bounded) #}

sizeOfGType :: forall a. GDALType a => a -> CInt
sizeOfGType _ =
  fromIntegral ({#call pure unsafe GetDataTypeSize as ^#} dt `div` 8)
  where dt = fromEnumC (dataType (Proxy :: Proxy a))

dataType :: GDALType a => Proxy a -> DataType
dataType = dataTypeInternal
{-# INLINE dataType #-}


------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class (Eq a, Storable a) => GDALType a where
  dataTypeInternal :: Proxy a -> DataType

unsafeCopyWords
  :: forall dst src. (GDALType dst, GDALType src)
  => Int -> Ptr dst -> Ptr src -> IO ()
unsafeCopyWords count dst src =
  {#call unsafe GDALCopyWords as ^#}
    (castPtr src)
    (fromEnumC (dataType (Proxy :: Proxy src)))
    (sizeOfGType (undefined :: src))
    (castPtr dst)
    (fromEnumC (dataType (Proxy :: Proxy dst)))
    (sizeOfGType (undefined :: dst))
    (fromIntegral count)
{-# INLINE unsafeCopyWords #-}

convertGType :: forall a b. (GDALType a, GDALType b) => a -> b
convertGType a
  | dataType (Proxy :: Proxy a) == dataType (Proxy :: Proxy b) = unsafeCoerce a
  | otherwise = unsafePerformIO $
    alloca $ \bPtr -> with a (unsafeCopyWords 1 bPtr) >> peek bPtr
{-# INLINE convertGType #-}

convertGTypeVector
  :: forall dst src. (GDALType dst, GDALType src)
  => St.Vector src -> St.Vector dst
convertGTypeVector src
  | dataType (Proxy :: Proxy src) == dataType (Proxy :: Proxy dst)
  = unsafeCoerce src
  | otherwise = unsafePerformIO $ do
      dst <- Stm.new (St.length src)
      St.unsafeThaw src >>= unsafeConvertGTypeMVector dst
      St.unsafeFreeze dst
{-# INLINE convertGTypeVector #-}

unsafeConvertGTypeMVector
  :: (GDALType dst, GDALType src)
  => Stm.IOVector dst -> Stm.IOVector src -> IO ()
unsafeConvertGTypeMVector dst src =
  Stm.unsafeWith dst (Stm.unsafeWith src . unsafeCopyWords (Stm.length dst))
{-# INLINE unsafeConvertGTypeMVector #-}

instance GDALType Word8 where
  dataTypeInternal _ = GDT_Byte

instance GDALType CUChar where
  dataTypeInternal _ = GDT_Byte

instance GDALType Word16 where
  dataTypeInternal _ = GDT_UInt16

instance GDALType CUShort where
  dataTypeInternal _ = GDT_UInt16

instance GDALType Word32 where
  dataTypeInternal _ = GDT_UInt32

instance GDALType CUInt where
  dataTypeInternal _ = GDT_UInt32

instance GDALType Int8 where
  dataTypeInternal _ = GDT_Byte

instance GDALType CSChar where
  dataTypeInternal _ = GDT_Byte

instance GDALType Int16 where
  dataTypeInternal _ = GDT_Int16

instance GDALType CShort where
  dataTypeInternal _ = GDT_Int16

instance GDALType Int32 where
  dataTypeInternal _ = GDT_Int32

instance GDALType CInt where
  dataTypeInternal _ = GDT_Int32

instance GDALType Float where
  dataTypeInternal _ = GDT_Float32

instance GDALType CFloat where
  dataTypeInternal _ = GDT_Float32

instance GDALType Double where
  dataTypeInternal _ = GDT_Float64

instance GDALType CDouble where
  dataTypeInternal _ = GDT_Float64


#ifdef STORABLE_COMPLEX
instance GDALType (Complex Int16) where
  dataTypeInternal _ = GDT_CInt16

instance GDALType (Complex Int32) where
  dataTypeInternal _ = GDT_CInt32

instance GDALType (Complex Float) where
  dataTypeInternal _ = GDT_CFloat32

instance GDALType (Complex Double) where
  dataTypeInternal _ = GDT_CFloat64
#endif

data SomeGDALType = forall a. GDALType a => SomeGDALType (Proxy a)

someGDALType :: DataType -> Maybe SomeGDALType
someGDALType dt =
  case dt of
    GDT_Byte     -> Just (SomeGDALType (Proxy :: Proxy Word8))
    GDT_UInt16   -> Just (SomeGDALType (Proxy :: Proxy Word16))
    GDT_UInt32   -> Just (SomeGDALType (Proxy :: Proxy Word32))
    GDT_Int16    -> Just (SomeGDALType (Proxy :: Proxy Int16))
    GDT_Int32    -> Just (SomeGDALType (Proxy :: Proxy Int32))
    GDT_Float32  -> Just (SomeGDALType (Proxy :: Proxy Float))
    GDT_Float64  -> Just (SomeGDALType (Proxy :: Proxy Double))
#ifdef STORABLE_COMPLEX
    GDT_CInt16   -> Just (SomeGDALType (Proxy :: Proxy (Complex Int16)))
    GDT_CInt32   -> Just (SomeGDALType (Proxy :: Proxy (Complex Int32)))
    GDT_CFloat32 -> Just (SomeGDALType (Proxy :: Proxy (Complex Float)))
    GDT_CFloat64 -> Just (SomeGDALType (Proxy :: Proxy (Complex Double)))
#else
    GDT_CInt16   -> Nothing
    GDT_CInt32   -> Nothing
    GDT_CFloat32 -> Nothing
    GDT_CFloat64 -> Nothing
#endif
    GDT_Unknown  -> Nothing
