{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.Types.Vector.Mutable(
    MVector(..)
  , IOVector
  , STVector

  , newAs
  , unsafeWithDataType
  , unsafeAsDataType
  , unsafeFromStorable
) where

import Control.DeepSeq ( NFData(rnf) )

import qualified Data.Vector.Storable.Mutable as St
import qualified Data.Vector.Generic.Mutable  as G
import Data.Vector.Storable.Internal

import Foreign.ForeignPtr

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import Foreign.Ptr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Storable (Storable)

import Control.Monad.Primitive
import Data.Primitive.Addr
import Data.Primitive.Types (Prim)

import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Ptr (Ptr(..))

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import GDAL.Internal.DataType

-- | Mutable 'GDALType'-based vectors
data MVector s a = MVector { mvLen         :: {-# UNPACK #-} !Int
                           , mvDataType    :: {-# UNPACK #-} !DataType
                           , mvPeekElemOff :: !(Ptr () -> Int -> IO a)
                           , mvPokeElemOff :: !(Ptr () -> Int -> a -> IO ())
                           , mvForeignPtr  :: {-# UNPACK #-} !(ForeignPtr ())
                           }
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _ _ _ _) = ()

instance GDALType a => G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = mvLen

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector _ dp pe po fp) =
    MVector m dp pe po (updPtr (`plusPtr` (j*sizeOfDataType dp)) fp)

  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector m dp _ _ fp) (MVector n dq _ _ fq)
    =  between p q (q `plusPtr` (n*sizeOfDataType dp))
    || between q p (p `plusPtr` (m*sizeOfDataType dq))
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq

  basicUnsafeNew = newAs (dataType (Proxy :: Proxy a))

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize (MVector n dp _ _ fp) =
    unsafePrimToPrim . withForeignPtr fp $ \(Ptr p) -> do
      let q = Addr p
      setAddr q byteSize (0 :: Word8)
    where
      byteSize :: Int
      byteSize = n * sizeOfDataType dp
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector _ _ pe _ fp) i
    = unsafePrimToPrim $ withForeignPtr fp $ \p -> pe p i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector _ _ _ po fp) i x
    = unsafePrimToPrim $ withForeignPtr fp $ \p -> po p i x

  {-# INLINE basicSet #-}
  basicSet = gdalVectorSet

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n dp _ _ fp) (MVector _ dq _ _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
        unsafeCopyWords p dp q dq n

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector n dp _ _ fp) (MVector _ dq _ _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
        unsafeCopyWords p dp q dq n

newAs
  :: (GDALType a, PrimMonad m)
  => DataType -> Int -> m (MVector (PrimState m) a)
newAs dt n
  | n < 0 = error $ "GDAL.Vector.new: negative length: " ++ show n
  | n > mx = error $ "GDAL.Vector.new: length too large: " ++ show n
  | otherwise = unsafePrimToPrim $ do
      fp <- mallocPlainForeignPtrBytes (n*size)
      return $ MVector n dt (gPeekElemOff dt) (gPokeElemOff dt) fp
  where
    size = sizeOfDataType dt
    mx = maxBound `quot` size :: Int
{-# INLINE newAs #-}


gdalVectorSet :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
{-# INLINE gdalVectorSet #-}
gdalVectorSet (MVector n dp _ po fp) x
  | n == 0 = return ()
  | otherwise = unsafePrimToPrim $
                case sizeOfDataType dp of
                  1 -> gdalVectorSetAsPrim (undefined :: Word8)
                  2 -> gdalVectorSetAsPrim (undefined :: Word16)
                  4 -> gdalVectorSetAsPrim (undefined :: Word32)
                  8 -> gdalVectorSetAsPrim (undefined :: Word64)
                  _ -> withForeignPtr fp $ \p -> do
                       po p 0 x
                       let do_set i
                             | 2*i < n = do
                                 copyBytes (p `plusPtr` o i) p (o i)
                                 do_set (2*i)
                             | otherwise =
                               copyBytes (p `plusPtr` o i) p (o (n-i))
                           o i = sizeOfDataType dp * i
                       do_set 1

  where
    {-# INLINE[0] gdalVectorSetAsPrim #-}
    gdalVectorSetAsPrim :: Prim b => b -> IO ()
    gdalVectorSetAsPrim y = withForeignPtr fp $ \(Ptr p) -> do
      po (Ptr p) 0 x
      let q = Addr p
      w <- readOffAddr q 0
      setAddr (q `plusAddr` sizeOfDataType dp) (n-1) (w `asTypeOf` y)


unsafeWithDataType
  :: IOVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithDataType (MVector _ dp _ _ fp) f = withForeignPtr fp (f dp)
{-# INLINE unsafeWithDataType #-}

unsafeAsDataType
  :: DataType -> IOVector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt (MVector n dp _ _ fp) f
  | dp == dt  = withForeignPtr fp f
  | otherwise =
    withForeignPtr fp $ \sPtr ->
    allocaBytesAligned (sizeOfDataType dt * n) (alignOfDataType dt) $ \dPtr ->
      unsafeCopyWords dPtr dt sPtr dp n >> f dPtr
{-# INLINE unsafeAsDataType #-}

unsafeFromStorable
  :: forall s dst src. (Storable src, GDALType src, GDALType dst)
  => St.MVector s src -> MVector s dst
unsafeFromStorable (St.MVector len fp) =
  MVector { mvLen         = len
          , mvDataType    = dt
          , mvPeekElemOff = gPeekElemOff dt
          , mvPokeElemOff = gPokeElemOff dt
          , mvForeignPtr  = castForeignPtr fp
          }
  where dt = dataType (Proxy :: Proxy src)
{-# INLINE unsafeFromStorable #-}
