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
  , checkCPLError
  , checkGDALCall
  , checkGDALCall_
  , withQuietErrorHandler
) where

{# context lib = "gdal" prefix = "CPL" #}

import Control.Monad (void, liftM)
import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Exception (
    Exception (..)
  , SomeException
  , throw
  , finally
  )
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.Storable (peekByteOff, peek)
import Foreign.Marshal.Utils (with)

-- work around  https://github.com/haskell/c2hs/issues/151
import qualified Foreign.C.Types as C2HSImp

import GDAL.Internal.Util (toEnumC, runBounded)
import GDAL.Internal.CPLString (peekEncodedCString)

#include "cpl_error.h"
#include "errorhandler.h"

{# enum CPLErr as ErrorType {upcaseFirstLetter} deriving (Eq, Show) #}

instance NFData ErrorType where
  rnf e = seq e ()


data GDALException
  = forall e. Exception e =>
    GDALBindingException !e
  | GDALException        { gdalErrType :: !ErrorType
                         , gdalErrNum  :: !ErrorNum
                         , gdalErrMsg  :: !Text}
  deriving Typeable

deriving instance Show GDALException

instance Exception GDALException

bindingExceptionToException :: Exception e=> e -> SomeException
bindingExceptionToException = toException . GDALBindingException

bindingExceptionFromException :: Exception e => SomeException -> Maybe e
bindingExceptionFromException x = do
  GDALBindingException a <- fromException x
  cast a


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

instance NFData ErrorNum where
  rnf e = seq e ()

checkGDALCall
  :: Exception e
  => (Maybe GDALException -> a -> Maybe e) -> IO a -> IO a
checkGDALCall isOk act = withErrorHandler $ \ stack -> do
  a <- act
  err <- peekLastError stack
  case isOk err a of
    Nothing -> return a
    Just e  -> throw e
{-# INLINE checkGDALCall #-}

checkGDALCall_
  :: Exception e
  => (Maybe GDALException -> a -> Maybe e) -> IO a -> IO ()
{-# INLINE checkGDALCall_ #-}
checkGDALCall_ isOk = void . checkGDALCall isOk

checkCPLError :: Text -> IO CInt -> IO ()
checkCPLError msg = checkGDALCall_ $ \mExc r ->
  case (mExc, toEnumC r) of
    (Nothing, CE_None) -> Nothing
    (Nothing, e)       -> Just (GDALException e AssertionFailed msg)
    (e, _)             -> e
{-# INLINE checkCPLError #-}

{#pointer ErrorCell #}
type ErrorStack = Ptr ErrorCell

peekLastError :: ErrorStack -> IO (Maybe GDALException)
peekLastError stack = do
  ec <- peek stack
  if ec == nullPtr
    then return Nothing
    else do
      msg <- peekEncodedCString =<< {#get ErrorCell->msg #} ec
      errClass <- liftM toEnumC ({#get ErrorCell->errClass#} ec)
      errNo <- liftM toEnumC ({#get ErrorCell->errNo#} ec)
      return (Just (GDALException errClass errNo msg))

withErrorHandler :: (ErrorStack -> IO a) -> IO a
withErrorHandler act = runBounded $ with nullPtr $ \stack ->
  ({#call unsafe push_error_handler as ^#} stack >> act stack)
    `finally` ({#call unsafe pop_error_handler as ^#} stack)
{-# INLINE withErrorHandler #-}


withQuietErrorHandler :: IO a -> IO a
withQuietErrorHandler a = runBounded ((pushIt >> a) `finally` popIt)
  where
    pushIt = {#call unsafe CPLPushErrorHandler as ^#} c_quietErrorHandler
    popIt  = {#call unsafe CPLPopErrorHandler as ^#}


foreign import ccall "cpl_error.h &CPLQuietErrorHandler"
  c_quietErrorHandler :: FunPtr (CInt -> CInt -> Ptr CChar -> IO ())
