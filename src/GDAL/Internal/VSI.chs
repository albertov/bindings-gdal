module GDAL.Internal.VSI (
  VSIException(..),
  getMemFileBuffer
) where

{#import GDAL.Internal.CPLError#}

import Control.Exception (Exception(..), bracketOnError)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import UnliftIO.Foreign (alloca, withCString, peek)
import Control.Monad (unless)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Marshal.Alloc (free)

#include "cpl_vsi.h"


data VSIException
  = InvalidMemFilename
  | OverflowError
  deriving Show

instance Exception VSIException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

-- | Get the contents of a in-memory file in the /vsimem/ namespace.
-- This function will unlink the in-memory file and take ownership of the
-- allocated memory. The buffer will be de-allocated when the returned
-- 'ByteString' is garbage collected
getMemFileBuffer :: String -> IO ByteString
getMemFileBuffer fname =
  alloca $ \sizePtr ->
  withCString fname $ \fnamePtr -> do
    bracketOnError
      ({#call unsafe VSIGetMemFileBuffer as ^#} fnamePtr sizePtr {#const TRUE#})
      free $ \bufPtr -> do
        bufSz <- peek sizePtr
        unless (bufPtr/=nullPtr) (throwBindingException InvalidMemFilename)
        unless (bufSz < fromIntegral (maxBound :: Int)) (throwBindingException OverflowError)
        unsafePackMallocCStringLen (castPtr bufPtr, fromIntegral bufSz)
