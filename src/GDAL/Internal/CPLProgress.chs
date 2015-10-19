{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.CPLProgress (
    ProgressFunPtr
  , ProgressFun
  , ContinueOrStop (..)
  , withProgressFun
) where

import Control.Monad (when)
import Control.Monad.Catch (bracket, catchAll, throwM, catchJust)
import Data.IORef (newIORef, readIORef, writeIORef)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr, nullPtr)

import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.CPLError (
    GDALException(gdalErrNum)
  , ErrorNum(UserInterrupt)
  )

#include "cpl_progress.h"

{#enum define ContinueOrStop {
    TRUE    as Continue
  , FALSE   as Stop
  } deriving (Eq, Bounded, Show) #}

type ProgressFun    = Double -> Maybe String -> IO ContinueOrStop
type CProgressFun   = CDouble -> CString -> Ptr () -> IO CInt
type ProgressFunPtr = FunPtr CProgressFun

instance Show ProgressFun where show _ = "ProgressFun"

withProgressFun :: Maybe ProgressFun -> (ProgressFunPtr -> IO c) -> IO (Maybe c)
withProgressFun Nothing  act = fmap Just (act c_dummyProgress)
withProgressFun (Just f) act = do
  excRef <- newIORef Nothing
  stoppedRef <- newIORef False
  let progressFunc progress cmsg _ = do
        msg <- if cmsg == nullPtr
                 then return Nothing
                 else fmap Just (peekCString cmsg)
        ret <- f (realToFrac progress) msg `catchAll` catcher
        when (ret == Stop) (writeIORef stoppedRef True)
        return (fromEnumC ret)
      catcher exc = writeIORef excRef (Just exc) >> return Stop
  ret <- catchJust
          (\e->if gdalErrNum e==UserInterrupt then Just e else Nothing)
          (bracket (c_wrapProgressFun progressFunc) freeHaskellFunPtr act)
          (const (writeIORef stoppedRef True >> return undefined))
  stopped <- readIORef stoppedRef
  let retWhenNoException
        | stopped   = return Nothing
        | otherwise = return (Just ret)
  readIORef excRef >>= maybe retWhenNoException throwM


foreign import ccall "wrapper"
  c_wrapProgressFun :: CProgressFun -> IO ProgressFunPtr

foreign import ccall "cpl_progress.h &GDALDummyProgress"
  c_dummyProgress :: ProgressFunPtr
