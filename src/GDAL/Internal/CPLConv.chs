{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.CPLConv (
    cplFree
  , cplMalloc
  , cplNewArray
  , cplFinalizerFree
  ) where

import Control.Exception (bracketOnError)
import Control.Monad (liftM)

import Foreign.C.Types (CULong(..))
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))

#include "cpl_conv.h"
#include "cpl_vsi.h"

-- | Like 'Foreign.Marshal.Alloc.malloc' but uses GDAL's allocator
--   (which might not be the same as ours).
--   Must be freed with 'cplFree' or let GDAL take care of it.
cplMalloc :: Storable a => IO (Ptr a)
cplMalloc = cplMallocArray 1
{-# INLINE cplMalloc #-}

-- | Like 'Foreign.Marshal.Array.mallocArray' but uses GDAL's allocator
--   (which might not be the same as ours).
--   Must be freed with 'cplFree' or let GDAL take care of it.
cplMallocArray :: forall a. Storable a => Int -> IO (Ptr a)
cplMallocArray nElems =
  liftM castPtr ({#call unsafe CPLMalloc as ^#} len)
  where len = fromIntegral (sizeOf (undefined :: a) * nElems)
{-# INLINE cplMallocArray #-}

-- | Like 'Foreign.Marshal.Alloc.free' but uses GDAL's de-allocator
--   (which might not be the same as ours).
cplFree :: Ptr a -> IO ()
cplFree p
  | p /= nullPtr = {#call unsafe VSIFree as ^#} (castPtr p)
  | otherwise    = return ()
{-# INLINE cplFree #-}

-- | Like 'Foreign.Marshal.Array.newArray' but uses GDAL's allocator
--   (which might not be the same as ours).
--   Must be freed with 'cplFree' or let GDAL take care of it.
cplNewArray :: Storable a => [a] -> IO (Ptr a)
cplNewArray [] = return nullPtr
cplNewArray l =
  bracketOnError (cplMallocArray (length l)) cplFree $ \ptr -> do
    mapM_ (\(i,v) -> pokeElemOff ptr i v) (zip [0..] l)
    return ptr
{-# INLINE cplNewArray #-}

-- | Like 'Foreign.Marshal.Alloc.finalizerFree' but for storage allocated
--   with 'cplMalloc', 'cplNewArray' or 'cplMallocArray'
foreign import ccall unsafe "cpl_conv.h &VSIFree"
  cplFinalizerFree :: FinalizerPtr a
