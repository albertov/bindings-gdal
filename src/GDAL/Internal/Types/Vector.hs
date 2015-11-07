{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.Types.Vector (
    Vector
  , MVector(..)
  , unsafeWithDataType
  , unsafeAsDataType
  , unsafeFromStorable
) where

import qualified Data.Vector.Storable         as St
import qualified Data.Vector.Generic          as G
import           GDAL.Internal.Types.Vector.Mutable ( MVector(..) )
import           Data.Vector.Storable.Internal

import Foreign.ForeignPtr
import Foreign.Storable (Storable)
import Foreign.Ptr
import Foreign.Marshal.Alloc (allocaBytesAligned)

import Control.DeepSeq ( NFData(rnf) )

import Control.Monad.Primitive

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import Text.Read     ( Read(..), readListPrecDefault )

import GDAL.Internal.DataType

-- | 'GDALType'-based vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !DataType
                       {-# UNPACK #-} !(ForeignPtr ())
        deriving ( Typeable )

instance NFData (Vector a) where
  rnf (Vector _ _ _) = ()

instance (Show a, GDALType a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, GDALType a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

type instance G.Mutable Vector = MVector

instance GDALType a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector n dp fp) = return $ Vector n dp fp

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector n dp fp) = return $ MVector n dp fp

  {-# INLINE basicLength #-}
  basicLength (Vector n _ _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (Vector _ dp fp) =
      Vector n dp (updPtr (`plusPtr` (i*sizeOfDataType dp)) fp)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector _ dp fp) i =
    return . unsafeInlineIO $ withForeignPtr fp $ \p -> gPeekElemOff dp p i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n dp fp) (Vector _ dq fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
        unsafeCopyWords p dp q dq n

  {-# INLINE elemseq #-}
  elemseq _ = seq

unsafeWithDataType
  :: GDALType a
  => Vector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithDataType (Vector _ dp fp) f = withForeignPtr fp (f dp)
{-# INLINE unsafeWithDataType #-}

unsafeAsDataType
  :: GDALType a
  => DataType -> Vector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt (Vector n dp fp) f
  | dp == dt  = withForeignPtr fp f
  | otherwise =
    withForeignPtr fp $ \sPtr ->
    allocaBytesAligned (sizeOfDataType dt * n) (alignOfDataType dt) $ \dPtr ->
      unsafeCopyWords dPtr dt sPtr dp n >> f dPtr
{-# INLINE unsafeAsDataType #-}

unsafeFromStorable
  :: forall dst src. (Storable src, GDALType dst, GDALType src)
  => St.Vector src -> Vector dst
unsafeFromStorable src =
  Vector len (dataType (Proxy :: Proxy src)) (castForeignPtr fp)
  where (fp, len) = St.unsafeToForeignPtr0 src
{-# INLINE unsafeFromStorable #-}
