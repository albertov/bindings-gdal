{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module OSGeo.GDAL.Types (
    GDALType (..)
  , Value(..)
  , Datatype(..)
  , fromValue
  , isNoData
) where

import Data.Word
import Data.Complex
import Data.Proxy
import Data.Int
import Foreign.Ptr
import Foreign.Storable

#include "gdal.h"

{# enum GDALDataType as Datatype {upcaseFirstLetter} deriving (Eq, Show) #}

class Storable a => GDALType a where
  datatype :: Proxy a -> Datatype
  -- | default nodata value when writing to bands with no datavalue set
  nodata   :: a
  -- | how to convert to double for use in setBandNodataValue
  nodataAsDouble :: a -> Double
  -- | how to convert from double for use with bandNodataValue
  nodataFromDouble :: Double -> a

  toValue          :: a -> Value a

data Value a where
  GByte     :: {-# UNPACK #-} !Word8            -> Value Word8
  GUInt16   :: {-# UNPACK #-} !Word16           -> Value Word16
  GUInt32   :: {-# UNPACK #-} !Word32           -> Value Word32
  GInt16    :: {-# UNPACK #-} !Int16            -> Value Int16
  GInt32    :: {-# UNPACK #-} !Int32            -> Value Int32
  GFloat32  :: {-# UNPACK #-} !Float            -> Value Float
  GFloat64  :: {-# UNPACK #-} !Double           -> Value Double
  GCInt16   :: {-# UNPACK #-} !(Complex Int16)  -> Value (Complex Int16)
  GCInt32   :: {-# UNPACK #-} !(Complex Int32)  -> Value (Complex Int32)
  GCFloat32 :: {-# UNPACK #-} !(Complex Float)  -> Value (Complex Float)
  GCFloat64 :: {-# UNPACK #-} !(Complex Double) -> Value (Complex Double)
  NoData    :: Value a
  Other     :: a                                -> Value a

deriving instance Show a => Show (Value a)
deriving instance Eq a   => Eq (Value a)

fromValue :: a -> Value a -> a
fromValue d NoData        = d
fromValue _ (GByte v)     = v
fromValue _ (GUInt16 v)   = v
fromValue _ (GUInt32 v)   = v
fromValue _ (GInt16 v)    = v
fromValue _ (GInt32 v)    = v
fromValue _ (GFloat32 v)  = v
fromValue _ (GFloat64 v)  = v
fromValue _ (GCInt16 v)   = v
fromValue _ (GCInt32 v)   = v
fromValue _ (GCFloat32 v) = v
fromValue _ (GCFloat64 v) = v
fromValue _ (Other v)     = v
{-# INLINE fromValue #-}

isNoData :: Value a -> Bool
isNoData NoData = True
isNoData _      = False
{-# INLINE isNoData #-}

instance Functor Value where
  fmap _ NoData = NoData
  fmap f a      = Other $ f $ fromValue undefined a

instance Applicative Value where
    pure = Other

    NoData  <*> _m      = NoData
    Other f <*> m       = fmap f m

    NoData    *> _m2    = NoData
    _m1       *> m2     = m2

instance Monad Value  where
    NoData    >>= _     = NoData
    x         >>= k     = k (fromValue undefined x)

    (>>) = (*>)

    return              = Other
    fail _              = NoData

instance GDALType a => Storable (Value a) where
  sizeOf _ = sizeOf (undefined :: Bool) + sizeOf (undefined :: a)
  alignment _ = 4

  {-# INLINE peek #-}
  peek p = do
            let p1 = (castPtr p::Ptr Bool) `plusPtr` 1
            t <- peek (castPtr p::Ptr Bool)
            if t
              then fmap toValue (peekElemOff (castPtr p1 :: Ptr a) 0)
              else return NoData

  {-# INLINE poke #-}
  poke p x = case x of
    NoData -> poke (castPtr p :: Ptr Bool) False
    a -> do
        poke (castPtr p :: Ptr Bool) True
        let p1 = (castPtr p :: Ptr Bool) `plusPtr` 1
        pokeElemOff (castPtr p1) 0 (fromValue undefined a)


instance GDALType Word8 where
  datatype _ = GDT_Byte
  nodata = maxBound
  nodataAsDouble = fromIntegral
  nodataFromDouble = round
  toValue = GByte
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Word16 where
  datatype _ = GDT_UInt16
  nodata = maxBound
  nodataAsDouble = fromIntegral
  nodataFromDouble = round
  toValue = GUInt16
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Word32 where
  datatype _ = GDT_UInt32
  nodata = maxBound
  nodataAsDouble = fromIntegral
  nodataFromDouble = round
  toValue = GUInt32
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Int16 where
  datatype _ = GDT_Int16
  nodata = minBound
  nodataAsDouble = fromIntegral
  nodataFromDouble = round
  toValue = GInt16
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Int32 where
  datatype _ = GDT_Int32
  nodata = minBound
  nodataAsDouble = fromIntegral
  nodataFromDouble = round
  toValue = GInt32
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Float where
  datatype _ = GDT_Float32
  nodata = 0/0
  nodataAsDouble = realToFrac
  nodataFromDouble = realToFrac
  toValue = GFloat32
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType Double where
  datatype _ = GDT_Float64
  nodata = 0/0
  nodataAsDouble = id
  nodataFromDouble = id
  toValue = GFloat64
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType (Complex Int16) where
  datatype _ = GDT_CInt16
  nodata = nodata :+ nodata
  nodataAsDouble = fromIntegral . realPart
  nodataFromDouble d = nodataFromDouble d :+ nodataFromDouble d
  toValue = GCInt16
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType (Complex Int32) where
  datatype _ = GDT_CInt32
  nodata = nodata :+ nodata
  nodataAsDouble = fromIntegral . realPart
  nodataFromDouble d = nodataFromDouble d :+ nodataFromDouble d
  toValue = GCInt32
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType (Complex Float) where
  datatype _ = GDT_CFloat32
  nodata = nodata :+ nodata
  nodataAsDouble = realToFrac . realPart
  nodataFromDouble d = nodataFromDouble d :+ nodataFromDouble d
  toValue = GCFloat32
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}

instance GDALType (Complex Double) where
  datatype _ = GDT_CFloat64
  nodata = nodata :+ nodata
  nodataAsDouble = realPart
  nodataFromDouble d = nodataFromDouble d :+ nodataFromDouble d
  toValue = GCFloat64
  {-# INLINE datatype #-}
  {-# INLINE nodataAsDouble #-}
  {-# INLINE nodataFromDouble #-}
  {-# INLINE toValue #-}
