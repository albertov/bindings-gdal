{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.CPLError (
    GDALException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException
  , setQuietErrorHandler
  , throwIfError
  , throwIfError_
  , throwBindingException
  , bindingExceptionToException
  , bindingExceptionFromException
) where

import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.Exception (Exception(..), SomeException, bracket, throw)
import Control.DeepSeq (NFData(rnf))
import Control.Monad.Catch (MonadThrow(throwM))

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Typeable (Typeable, cast)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)

import GDAL.Internal.Util (toEnumC)

#include "cpl_error.h"

type ErrorHandler = CInt -> CInt -> CString -> IO ()

data GDALException
  = forall e. Exception e =>
    GDALBindingException !e
  | GDALException        { gdalErrType :: !ErrorType
                         , gdalErrNum  :: !ErrorNum
                         , gdalErrMsg  :: !String
                         }
  deriving Typeable

deriving instance Show GDALException

instance Exception GDALException

bindingExceptionToException :: Exception e => e -> SomeException
bindingExceptionToException = toException . GDALBindingException

bindingExceptionFromException :: Exception e => SomeException -> Maybe e
bindingExceptionFromException x = do
  GDALBindingException a <- fromException x
  cast a


instance NFData GDALException where
  rnf a = a `seq` () -- All fields are already strict so no need to rnf them

throwBindingException :: (MonadThrow m, Exception e) => e -> m a
throwBindingException = throwM . bindingExceptionToException

isGDALException :: SomeException -> Bool
isGDALException e = isJust (fromException e :: Maybe GDALException)

isBindingException :: SomeException -> Bool
isBindingException e
  = case fromException e of
      Just (GDALBindingException _) -> True
      _                             -> False

{# enum CPLErr as ErrorType {upcaseFirstLetter} deriving (Eq, Show) #}

{#enum define ErrorNum {
    CPLE_None            as None
  , CPLE_AppDefined      as AppDefined
  , CPLE_OutOfMemory     as OutOfMemory
  , CPLE_FileIO          as FileIO
  , CPLE_OpenFailed      as OpenFailed
  , CPLE_IllegalArg      as IllegalArg
  , CPLE_NotSupported    as NotSupported
  , CPLE_AssertionFailed as AssertionFailed
  , CPLE_NoWriteAccess   as NoWriteAccess
  , CPLE_UserInterrupt   as UserInterrupt 
  , CPLE_ObjectNull      as ObjectNull
  } deriving (Eq, Bounded, Show) #}

instance NFData ErrorType where
  rnf a = a `seq` ()


foreign import ccall "cpl_error.h &CPLQuietErrorHandler"
  c_quietErrorHandler :: FunPtr ErrorHandler

foreign import ccall "cpl_error.h CPLSetErrorHandler"
  setErrorHandler :: FunPtr ErrorHandler -> IO (FunPtr ErrorHandler)

setQuietErrorHandler :: IO (FunPtr ErrorHandler)
setQuietErrorHandler = setErrorHandler c_quietErrorHandler

foreign import ccall "cpl_error.h CPLPushErrorHandler"
  c_pushErrorHandler :: FunPtr ErrorHandler -> IO ()

foreign import ccall "cpl_error.h CPLPopErrorHandler"
  c_popErrorHandler :: IO ()

foreign import ccall safe "wrapper"
  mkErrorHandler :: ErrorHandler -> IO (FunPtr ErrorHandler)

throwIfError_ :: String -> IO a -> IO ()
throwIfError_ prefix act = throwIfError prefix act >> return ()

throwIfError :: String -> IO a -> IO a
throwIfError prefix act = do
    ref <- newIORef Nothing
    let mkHandler = mkErrorHandler $ \err errno cmsg -> do
          msg <- peekCString cmsg
          writeIORef ref $ Just $ GDALException (toEnumC err)
                                                (toEnumC errno)
                                                (prefix ++ ": " ++ msg)
    bracket mkHandler freeHaskellFunPtr $ \handler -> do
      ret <- runBounded $ bracket (c_pushErrorHandler handler)
                                  (const c_popErrorHandler)
                                  (const act)
      readIORef ref >>= maybe (return ret) throw
  where
    runBounded 
      | rtsSupportsBoundThreads = runInBoundThread
      | otherwise               = id
