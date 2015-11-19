{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.DataType.Instances () where

import GDAL.Internal.Types.Value (Masked(..))
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType

import Control.Arrow ((&&&))

import Data.Int (Int8, Int16, Int32)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)

import Foreign.C.Types

#if MIN_VERSION_base(4,8,0)
import Data.Complex (Complex((:+)), realPart, imagPart)
#else
import Data.Complex (Complex((:+)))
realPart, imagPart :: Complex t -> t
realPart (a :+ _) = a
imagPart (_ :+ a) = a
#endif


instance GDALType a => GDALType (DynType a) where
  type PrimType (DynType a) = a
  dataType _        = dataType (Proxy :: Proxy a)
  gToIntegral       = gToIntegral . unDynType
  gToIntegralPair   = gToIntegralPair . unDynType
  gToReal           = gToReal . unDynType
  gToRealPair       = gToRealPair . unDynType
  gFromIntegral     = DynType . gFromIntegral
  gFromIntegralPair = DynType . gFromIntegralPair
  gFromReal         = DynType . gFromReal
  gFromRealPair     = DynType . gFromRealPair
  {-# INLINE dataType          #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
instance GDALType a => Masked (DynType a) where
  type BaseMVector (DynType a) = GV.MVector
  type BaseVector (DynType a)  = GV.Vector

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
instance Masked Word8 where
  type BaseMVector Word8 = GV.MVector
  type BaseVector Word8  = GV.Vector


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
instance Masked Word16 where
  type BaseMVector Word16 = GV.MVector
  type BaseVector Word16  = GV.Vector

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
instance Masked Word32 where
  type BaseMVector Word32 = GV.MVector
  type BaseVector Word32  = GV.Vector

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
instance Masked Int8 where
  type BaseMVector Int8 = GV.MVector
  type BaseVector Int8  = GV.Vector

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
instance Masked Int16 where
  type BaseMVector Int16 = GV.MVector
  type BaseVector Int16  = GV.Vector

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
instance Masked Int32 where
  type BaseMVector Int32 = GV.MVector
  type BaseVector Int32  = GV.Vector

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
instance Masked Float where
  type BaseMVector Float = GV.MVector
  type BaseVector Float  = GV.Vector

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
instance Masked Double where
  type BaseMVector Double = GV.MVector
  type BaseVector Double  = GV.Vector

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
instance Masked CDouble where
  type BaseMVector CDouble = GV.MVector
  type BaseVector CDouble  = GV.Vector


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
instance Masked (Complex Int16) where
  type BaseMVector (Complex Int16) = GV.MVector
  type BaseVector (Complex Int16)  = GV.Vector



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
instance Masked (Complex Int32) where
  type BaseMVector (Complex Int32) = GV.MVector
  type BaseVector (Complex Int32)  = GV.Vector

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
instance Masked (Complex Float) where
  type BaseMVector (Complex Float) = GV.MVector
  type BaseVector (Complex Float)  = GV.Vector

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
instance Masked (Complex Double) where
  type BaseMVector (Complex Double) = GV.MVector
  type BaseVector (Complex Double)  = GV.Vector
