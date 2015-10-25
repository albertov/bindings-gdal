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
import Control.Monad.Catch (
    Exception
  , SomeException
  , bracket
  , catchAll
  , throwM
  , try
  )
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr, nullPtr)

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)

#include "cpl_progress.h"

{#enum define ContinueOrStop {
    TRUE    as Continue
  , FALSE   as Stop
  } deriving (Eq, Bounded, Show) #}

type ProgressFun    = Double -> Maybe String -> IO ContinueOrStop
type CProgressFun   = CDouble -> CString -> Ptr () -> IO CInt
type ProgressFunPtr = FunPtr CProgressFun

instance Show ProgressFun where show _ = "ProgressFun"

withProgressFun
  :: Exception e
  => e -> Maybe ProgressFun -> (ProgressFunPtr -> GDAL s a) -> GDAL s a
withProgressFun _ Nothing  act = act c_dummyProgress
withProgressFun stopExc (Just f) act = do
  excRef <- liftIO $ newIORef Nothing
  stoppedRef <- liftIO $ newIORef False
  let progressFunc progress cmsg _ = do
        msg <- if cmsg == nullPtr
                 then return Nothing
                 else fmap Just (peekCString cmsg)
        ret <- f (realToFrac progress) msg `catchAll` catcher
        when (ret == Stop) (writeIORef stoppedRef True)
        return (fromEnumC ret)
      catcher exc = writeIORef excRef (Just exc) >> return Stop
  mRet <- try $ bracket
            (liftIO (c_wrapProgressFun progressFunc))
            (liftIO . freeHaskellFunPtr)
            act
  stopped <- liftIO $ readIORef stoppedRef
  mExc <- liftIO $ readIORef excRef
  case (stopped, mRet, mExc) of
    -- An error ocurred inside the progress function
    (True,  _       , Just e)  -> throwM e
    -- The progress function stopped the action
    (True,  _       , Nothing) -> throwM stopExc
    -- action completed without interruptions
    (False, Right v , Nothing) -> return v
    -- action threw an exception
    (False, Left  e , Nothing) -> throwM (e :: SomeException)
    _                          -> error "withProgressFun: shouldn't happen"


foreign import ccall "wrapper"
  c_wrapProgressFun :: CProgressFun -> IO ProgressFunPtr

foreign import ccall "cpl_progress.h &GDALDummyProgress"
  c_dummyProgress :: ProgressFunPtr
