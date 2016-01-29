{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module GDAL.Internal.CPLProgress (
    ProgressFunPtr
  , ProgressFun
  , ProgressException (..)
  , ContinueOrStop (..)
  , HasProgressFun (..)
  , withProgressFun
  , isProgressFunException
  , isInterruptedException
) where

import Control.Monad (when)
import Control.Exception (ErrorCall(..))
import Control.Monad.Catch (
    Exception (..)
  , SomeException
  , bracket
  , catchAll
  , throwM
  , try
  )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr, nullPtr)

import Lens.Micro (Lens')


import GDAL.Internal.CPLError (
    bindingExceptionToException
  , bindingExceptionFromException
  , throwBindingException
  )
import GDAL.Internal.Util (fromEnumC)

#include "cpl_progress.h"

class HasProgressFun o a | o -> a where
  progressFun :: Lens' o a

data ProgressException
  = forall e. Exception e =>
    ProgressFunException !e
  | Interrupted          !Text
  deriving Typeable

deriving instance Show ProgressException

instance Exception ProgressException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

isProgressFunException :: SomeException -> Bool
isProgressFunException e =
  case fromException e of
    Just (ProgressFunException _) -> True
    _                             -> False


isInterruptedException :: SomeException -> Bool
isInterruptedException e =
  case fromException e of
    Just (Interrupted _) -> True
    _                    -> False

{#enum define ContinueOrStop {
    TRUE    as Continue
  , FALSE   as Stop
  } deriving (Eq, Bounded, Show) #}

type ProgressFun    = Double -> Maybe String -> IO ContinueOrStop
type CProgressFun   = CDouble -> CString -> Ptr () -> IO CInt
type ProgressFunPtr = FunPtr CProgressFun

instance Show ProgressFun where show _ = "ProgressFun"

withProgressFun
  :: Text -> Maybe ProgressFun -> (ProgressFunPtr -> IO a) -> IO a
withProgressFun _      Nothing  act = act c_dummyProgress
withProgressFun ctxMsg (Just f) act = do
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
  mRet <- try $ bracket (c_wrapProgressFun progressFunc) freeHaskellFunPtr act
  stopped <- readIORef stoppedRef
  mExc <- readIORef excRef
  case (stopped, mRet, mExc) of
    -- An error ocurred inside the progress function
    (True,  _       , Just e)  -> throwM (ProgressFunException e)
    -- The progress function stopped the action
    (True,  _       , Nothing) -> throwM (Interrupted ctxMsg)
    -- action completed without interruptions
    (False, Right v , Nothing) -> return v
    -- action threw an exception
    (False, Left  e , Nothing) -> throwM (e :: SomeException)
    _  -> throwBindingException (ErrorCall "withProgressFun: shouldn't happen")


foreign import ccall "wrapper"
  c_wrapProgressFun :: CProgressFun -> IO ProgressFunPtr

foreign import ccall "cpl_progress.h &GDALDummyProgress"
  c_dummyProgress :: ProgressFunPtr
