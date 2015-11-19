{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType
  , DataTypeMismatch (..)
  , DynType (..)
  , Vector (..)
  , MVector (..)

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

import Control.Exception (Exception(..))
import Control.Monad.Primitive (PrimMonad(PrimState), RealWorld)

import Data.Primitive.MachDeps
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))

import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as M

import Language.Haskell.TH.Syntax (Lift)

import Foreign.Ptr (Ptr)


import GDAL.Internal.CPLError (
    bindingExceptionFromException
  , bindingExceptionToException
  )
import GDAL.Internal.Types.Pair (Pair)
import GDAL.Internal.Types.Value (Masked(..))

data DataTypeMismatch =
  DataTypeMismatch { rasterDt :: !DataType, expectedDt :: !DataType}
  deriving (Typeable, Show, Eq)

instance Exception DataTypeMismatch where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException


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
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}

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

class (Masked a, Vector (BaseVector a) a) => GDALType a where
  dataType :: Proxy a -> DataType

  gToIntegral   :: Integral b => a -> b
  gFromIntegral :: Integral b => b -> a

  gToReal   :: RealFrac b => a -> b
  gFromReal :: RealFrac b => b -> a

  gToRealPair   :: RealFrac b => a -> Pair b
  gFromRealPair :: RealFrac b => Pair b -> a

  gToIntegralPair   :: Integral b => a -> Pair b
  gFromIntegralPair :: Integral b => Pair b -> a




class (MVector (G.Mutable v) a, G.Vector v a) => Vector v a where
  gUnsafeAsDataType   :: DataType -> v a  -> (Ptr () -> IO b) -> IO b
  gUnsafeWithDataType :: v a -> (DataType -> Ptr () -> IO b) -> IO b


class M.MVector v a => MVector v a where
  gNewAs :: PrimMonad m => DataType -> Int  -> m (v (PrimState m) a)
  gUnsafeWithDataTypeM :: v RealWorld a -> (DataType -> Ptr () -> IO b) -> IO b



newtype DynType a = DynType { unDynType :: a}
  deriving ( Eq, Show, Enum, Bounded, Real, Num, Ord, Integral, Fractional
           , RealFrac , RealFloat, Floating, Lift, Functor)
