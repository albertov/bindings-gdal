{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module GDAL.Internal.CPLError (
    GDALException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException
  , throwBindingException
  , bindingExceptionToException
  , bindingExceptionFromException
  , withErrorHandler
  , popLastError
  , checkCPLError
  , checkGDALCall
  , checkGDALCall_
) where

{# context lib = "gdal" prefix = "CPL" #}

import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (void, liftM)
import Control.Monad.Catch (
    Exception (..)
  , SomeException
  , MonadCatch
  , MonadMask
  , MonadThrow (throwM)
  , mask
  , onException
  , bracket
  , finally
  )
import Control.Monad.IO.Class (MonadIO(..))

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Data.Monoid (mconcat)

import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peekByteOff)

-- work around  https://github.com/haskell/c2hs/issues/151
import qualified Foreign.C.Types as C2HSImp

import Foreign.Ptr (Ptr)

import GDAL.Internal.Util (toEnumC, peekEncodedCString)

#include "cpl_error.h"
#include "cbits.h"

{# enum CPLErr as ErrorType {upcaseFirstLetter} deriving (Eq, Show) #}


data GDALException
  = forall e. (Exception e, NFData e) =>
    GDALBindingException !e
  | GDALException        { gdalErrType :: !ErrorType
                         , gdalErrNum  :: !ErrorNum
                         , gdalErrMsg  :: !Text}
  deriving Typeable

deriving instance Show GDALException

instance Exception GDALException

bindingExceptionToException
  :: (Exception e, NFData e) => e -> SomeException
bindingExceptionToException = toException . GDALBindingException

bindingExceptionFromException
  :: (Exception e, NFData e) => SomeException -> Maybe e
bindingExceptionFromException x = do
  GDALBindingException a <- fromException x
  cast a


instance NFData GDALException where
  rnf (GDALBindingException e) = rnf e
  rnf (GDALException e n m) = rnf e `seq` rnf n `seq` rnf m `seq` ()

throwBindingException :: (MonadThrow m, Exception e, NFData e) => e -> m a
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

instance NFData ErrorNum where
  rnf a = a `seq` ()


checkGDALCall
  :: (MonadMask m, MonadIO m, Exception e)
  => (Maybe GDALException -> a -> Maybe e) -> m a -> m a
checkGDALCall isOk act = mask $ \restore -> do
  liftIO $ clearErrors
  a <- restore act `onException` (liftIO popLastError)
  err <- liftIO popLastError
  case isOk err a of
    Nothing -> return a
    Just e  -> throwM e
{-# INLINE checkGDALCall #-}

checkGDALCall_
  :: (MonadMask m, MonadIO m, Exception e)
  => (Maybe GDALException -> a -> Maybe e) -> m a -> m ()
{-# INLINE checkGDALCall_ #-}
checkGDALCall_ isOk = void . checkGDALCall isOk

checkCPLError :: IO CInt -> IO ()
checkCPLError = checkGDALCall_ $ \mExc r ->
  case (mExc, toEnumC r) of
    (Nothing, CE_None) -> Nothing
    (Nothing, e)       -> Just (GDALException e AssertionFailed "checkCPLError")
    (e, _)             -> e
{-# INLINE checkCPLError #-}

{#pointer ErrorCell #}

popLastError :: IO (Maybe GDALException)
popLastError =
  bracket {#call unsafe pop_last as ^#}
          {#call unsafe destroy_ErrorCell as ^#} $ \ec -> do
  if ec == nullPtr
    then return Nothing
    else do
      msg <- peekEncodedCString =<< {#get ErrorCell->msg #} ec
      errClass <- liftM toEnumC ({#get ErrorCell->errClass#} ec)
      errNo <- liftM toEnumC ({#get ErrorCell->errNo#} ec)
      return (Just (GDALException errClass errNo msg))

clearErrors :: IO ()
clearErrors = {#call unsafe clear_stack as ^#}

withErrorHandler :: IO a -> IO a
withErrorHandler act = runBounded $
  ({#call unsafe push_error_handler as ^#} >> act)
    `finally` {#call unsafe pop_error_handler as ^#}
  where
    runBounded
      | rtsSupportsBoundThreads = runInBoundThread
      | otherwise               = id
