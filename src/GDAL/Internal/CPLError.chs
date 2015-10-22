{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.CPLError (
    GDALException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , CErrorHandler
  , isGDALException
  , isBindingException
  , throwIfError
  , throwIfError_
  , throwBindingException
  , bindingExceptionToException
  , bindingExceptionFromException
  , collectMessages
  , withErrorHandler
) where

{# context lib = "gdal" prefix = "CPL" #}

import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.Exception (Exception(..), SomeException, bracket, throw)
import Control.DeepSeq (NFData(rnf))
import Control.Monad.Catch (MonadThrow(throwM))

import Data.IORef (newIORef, readIORef, modifyIORef')
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
                         , gdalErrMsg  :: !Text
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

throwIfError_ :: Text -> IO a -> IO ()
throwIfError_ prefix act = throwIfError prefix act >> return ()

throwIfError :: Text -> IO a -> IO a
throwIfError prefix act = do
  (ret, msgs) <- collectMessages act
  case msgs of
    []          -> return ret
    ((a,b,c):_) -> throw (GDALException a b (mconcat [prefix,": ", c]))

collectMessages :: IO a -> IO (a, [(ErrorType,ErrorNum,Text)])
collectMessages act = do
  msgsRef <- newIORef []
  let handler err errno cmsg = do
        msg <- peekEncodedCString cmsg
        modifyIORef' msgsRef ((:) (toEnumC err, toEnumC errno, msg))
  ret <- withErrorHandler handler act
  msgs <- readIORef msgsRef
  return (ret, msgs)

withErrorHandler :: CErrorHandler -> IO a -> IO a
withErrorHandler eh act =
  bracket (mkErrorHandler eh) freeHaskellFunPtr $ \h -> do
    runBounded $ bracket ({#call unsafe PushErrorHandler as ^#} h)
                         (const {#call unsafe PopErrorHandler as ^#})
                         (const act)
  where
    runBounded
      | rtsSupportsBoundThreads = runInBoundThread
      | otherwise               = id
