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

import Foreign.C.Types
import Foreign.C.String (CString)
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

getConfigOption :: ByteString -> IO (Maybe ByteString)
getConfigOption key = useAsCString key $ \keyPtr -> do
  valPtr <- {#call unsafe CPLGetConfigOption as ^#} keyPtr nullPtr
  if valPtr == nullPtr then return Nothing else liftM Just (packCString valPtr)

setConfigOptionWith
  :: (CString -> CString -> IO ()) -> ByteString -> Maybe ByteString -> IO ()
setConfigOptionWith fun key mVal =
  useAsCString key $ \keyPtr ->
  case mVal of
    Just val -> useAsCString val (fun keyPtr)
    Nothing  -> fun keyPtr nullPtr

setConfigOption :: ByteString -> Maybe ByteString -> IO ()
setConfigOption = setConfigOptionWith {#call unsafe CPLSetConfigOption as ^#}

setThreadLocalConfigOption :: ByteString -> Maybe ByteString -> IO ()
setThreadLocalConfigOption =
  setConfigOptionWith {#call unsafe CPLSetThreadLocalConfigOption as ^#}


withConfigOption
  :: (MonadMask m, MonadBaseControl IO m, MonadIO m)
  => ByteString -> Maybe ByteString -> m a -> m a
withConfigOption key val = runBounded . bracket enter exit . const
  where
    enter = liftIO $ do
      curVal <- getConfigOption key
      setThreadLocalConfigOption key val
      return curVal
    exit = liftIO . setThreadLocalConfigOption key
