{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Internal.CPLConv (
    cplFree
  , cplMalloc
  , cplNewArray
  , cplFinalizerFree
  , setConfigOption
  , getConfigOption
  , withConfigOption
  ) where

import GDAL.Internal.Util (runBounded)
import Control.Exception (bracketOnError)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Catch (MonadMask, bracket)

import Data.ByteString.Char8 (ByteString, packCString, useAsCString)

import Foreign.C.Types (CULong(..), CChar(..))
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
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

{#fun unsafe CPLGetConfigOption as getConfigOption
   { useAsCString* `ByteString'} -> `ByteString' packCString* #}

{#fun unsafe CPLSetConfigOption as setConfigOption
   { useAsCString* `ByteString', useAsCString* `ByteString' } -> `()' #}

{#fun unsafe CPLSetThreadLocalConfigOption as setThreadLocalConfigOption
   { useAsCString* `ByteString', useAsCString* `ByteString' } -> `()' #}

withConfigOption
  :: (MonadMask m, MonadBaseControl IO m, MonadIO m)
  => ByteString -> ByteString -> m a -> m a
withConfigOption key val = runBounded . bracket enter exit . const
  where
    enter = liftIO $ do
      curVal <- getConfigOption key
      setThreadLocalConfigOption key val
      return curVal
    exit = liftIO . setThreadLocalConfigOption key
