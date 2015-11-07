{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.Types.Vector (
    Vector (..)
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
data Vector a = Vector { vLen         :: {-# UNPACK #-} !Int
                       , vDataType    :: {-# UNPACK #-} !DataType
                       , vPeekElemOff :: !(Ptr () -> Int -> IO a)
                       , vPokeElemOff :: !(Ptr () -> Int -> a -> IO ())
                       , vForeignPtr  :: {-# UNPACK #-} !(ForeignPtr ())
                       }
        deriving ( Typeable )

instance NFData (Vector a) where
  rnf (Vector _ _ _ _ _) = ()

instance (GDALType a, Show a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (GDALType a, Read a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

type instance G.Mutable Vector = MVector

instance GDALType a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector n dp pe po fp) = return $ Vector n dp pe po fp

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector n dp pe po fp) = return $ MVector n dp pe po fp

  {-# INLINE basicLength #-}
  basicLength = vLen

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (Vector _ dp pe po fp) =
      Vector n dp pe po (updPtr (`plusPtr` (i*sizeOfDataType dp)) fp)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector _ _ pe _ fp) i =
    return . unsafeInlineIO $ withForeignPtr fp $ \p -> pe p i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n dp _ _ fp) (Vector _ dq _ _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
        unsafeCopyWords p dp q dq n

  {-# INLINE elemseq #-}
  elemseq _ = seq

unsafeWithDataType
  :: Vector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithDataType (Vector _ dp _ _ fp) f = withForeignPtr fp (f dp)
{-# INLINE unsafeWithDataType #-}

unsafeAsDataType
  :: DataType -> Vector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt (Vector n dp _ _ fp) f
  | dp == dt  = withForeignPtr fp f
  | otherwise =
    withForeignPtr fp $ \sPtr ->
    allocaBytesAligned (sizeOfDataType dt * n) (alignOfDataType dt) $ \dPtr ->
      unsafeCopyWords dPtr dt sPtr dp n >> f dPtr
{-# INLINE unsafeAsDataType #-}

unsafeFromStorable
  :: forall dst src. (Storable src, GDALType src, GDALType dst)
  => St.Vector src -> Vector dst
unsafeFromStorable src =
  Vector { vLen         = len
         , vDataType    = dt
         , vPeekElemOff = gPeekElemOff dt
         , vPokeElemOff = gPokeElemOff dt
         , vForeignPtr  = castForeignPtr fp
         }
  where (fp, len) = St.unsafeToForeignPtr0 src
        dt        = dataType (Proxy :: Proxy src)
{-# INLINE unsafeFromStorable #-}
