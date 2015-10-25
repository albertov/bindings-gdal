{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.CPLError (
    GDALException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , ErrorStack
  , CErrorHandler
  , isGDALException
  , isBindingException
  , throwBindingException
  , bindingExceptionToException
  , bindingExceptionFromException
  , errorCollector
  , popLastError
  , clearErrors
  , withErrorHandler
  , checkReturns
  , checkReturns_
) where

{# context lib = "gdal" prefix = "CPL" #}

import Control.Exception (Exception(..), SomeException, finally)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(..))

import Data.IORef (IORef, newIORef, atomicModifyIORef', writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Data.Monoid (mconcat)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CChar(..))

-- work around  https://github.com/haskell/c2hs/issues/151
import qualified Foreign.C.Types as C2HSImp

import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)

import GDAL.Internal.Util (toEnumC, peekEncodedCString)

#include "cpl_error.h"

{# enum CPLErr as ErrorType {upcaseFirstLetter} deriving (Eq, Show) #}


type CErrorHandler = CInt -> CInt -> CString -> IO ()

{#pointer ErrorHandler#}

data GDALException
  = forall e. Exception e =>
    GDALBindingException !e
  | GDALException        { gdalErrType :: !ErrorType
                         , gdalErrNum  :: !ErrorNum
                         , gdalErrMsg  :: !Text}
  | ReturnsError
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

foreign import ccall safe "wrapper"
  mkErrorHandler :: CErrorHandler -> IO ErrorHandler

checkReturns
  :: (Eq a, MonadThrow m)
  => (a -> Bool) -> m a -> m a
checkReturns isOk act = do
  a <- act
  if isOk a then return a else throwM ReturnsError

checkReturns_
  :: (Eq a, MonadThrow m)
  => (a -> Bool) -> m a -> m ()
checkReturns_ isOk = void . checkReturns isOk

newtype ErrorStack = ErrorStack (IORef [GDALException])

popLastError :: ErrorStack -> IO (Maybe GDALException)
popLastError (ErrorStack ref) =
  atomicModifyIORef' ref $ \errors ->
    case errors of
      []     -> ([], Nothing)
      (x:xs) -> (xs, Just x)

clearErrors :: ErrorStack -> IO ()
clearErrors (ErrorStack ref) = writeIORef ref []

errorCollector
  :: IO (ErrorStack, IO a -> IO a)
errorCollector = do
  msgsRef <- newIORef []
  let handler err errno cmsg = do
        msg <- peekEncodedCString cmsg
        atomicModifyIORef' msgsRef $ \errors ->
          ((GDALException (toEnumC err) (toEnumC errno) msg):errors, ())
  return (ErrorStack msgsRef, withErrorHandler handler)

withErrorHandler :: CErrorHandler -> IO a -> IO a
withErrorHandler eh act = do
  h <- mkErrorHandler eh
  ({#call unsafe PushErrorHandler as ^#} h >> act)
    `finally` ({#call unsafe PopErrorHandler as ^#} >> freeHaskellFunPtr h)
