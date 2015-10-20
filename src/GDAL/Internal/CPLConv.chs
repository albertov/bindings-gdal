{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.CPLConv (
    cplMalloc
  , cplMallocArray
  , cplNew
  , cplFree
  , listToArray
  ) where

import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..))
import Control.Monad (liftM, when)

import Data.Typeable (Typeable)

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.C.Types (CULong(..))

import GDAL.Internal.CPLError

#include "cpl_vsi.h"

data MallocFailed = MallocFailed
  deriving (Typeable, Show, Eq)

instance NFData MallocFailed where
  rnf a = a `seq` ()

instance Exception MallocFailed where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

cplMalloc :: forall a. Storable a => IO (Ptr a)
cplMalloc = cplMallocArray 1
{-# INLINE cplMalloc #-}

cplMallocArray :: forall a. Storable a => Int -> IO (Ptr a)
cplMallocArray nElems = do
  ptr <- liftM castPtr ({#call unsafe VSIMalloc as ^#} len)
  when (ptr == nullPtr) (throwBindingException MallocFailed)
  return ptr
  where len = fromIntegral (sizeOf (undefined :: a) * nElems)
{-# INLINE cplMallocArray #-}

cplNew :: forall a. Storable a => a -> IO (Ptr a)
cplNew v = cplMalloc >>= (\p -> poke p v >> return p)
{-# INLINE cplNew #-}

cplFree :: Ptr a -> IO ()
cplFree = {#call unsafe VSIFree as ^#} . castPtr
{-# INLINE cplFree #-}


listToArray :: Storable a => [a] -> IO (Ptr a)
listToArray [] = return nullPtr
listToArray l = do
  ptr <- cplMallocArray (length l)
  mapM_ (\(i,v) -> pokeElemOff ptr i v) (zip [0..] l)
  return ptr
{-# INLINE listToArray #-}
