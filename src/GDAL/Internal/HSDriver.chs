{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GDAL.Internal.HSDriver (
    HSDriver (..)
  , HSDriverOpen (..)
  , createDriver
  , withDriver
  , registerDriver
  , deregisterDriver
  , deleteDriver
) where

#include "gdal.h"
#include "driver.h"

{#import GDAL.Internal.GDAL#}
import GDAL.Internal.Types (GDAL)
import GDAL.Internal.Common
import GDAL.Internal.HSDataset

import Control.Monad
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Catch ( SomeException, MonadMask, bracket, try  )

import Data.ByteString.Char8 ( ByteString, packCString, useAsCString )
import Foreign.C
import Foreign.Ptr ( nullPtr )
import System.IO ( hPutStrLn, stderr )

data HSDriver = HSDriver
  { hsdName     :: DriverName
  , hsdIdentify :: ByteString -> IO Bool
  , hsdOpen     :: ByteString -> IO HSDriverOpen
  }

data HSDriverOpen where
  HsdDatasetH ::           !DatasetH               -> HSDriverOpen
  HsdDataset  :: forall s. !(GDAL s (HSDataset s)) -> HSDriverOpen
  HsdError    ::                                      HSDriverOpen
  

{#pointer GDALOpenInfoH#}
{#pointer HsDriverIdentify#}
{#pointer HsDriverOpen#}

foreign import ccall "wrapper" c_wrapHsDriverIdentify ::
  (GDALOpenInfoH -> IO CInt) -> IO HsDriverIdentify
foreign import ccall "wrapper" c_wrapHsDriverOpen ::
  (GDALOpenInfoH -> IO DatasetH) -> IO HsDriverOpen

createDriver
  :: MonadIO m => HSDriver -> m (Driver t)
createDriver HSDriver{..} = liftIO $ useAsCString name $ \namePtr -> do

  identify <- c_wrapHsDriverIdentify $ \oinfo -> do
    fname <- openInfoFilename oinfo
    eValid <- try (hsdIdentify fname)
    case eValid of
      Right True -> return true
      Right False -> return false
      Left (e :: SomeException) -> do
        hPutStrLn stderr
          ("ERROR: Unhandled exception in HSDriver.identify: " ++ show e)
        return false

  open <- c_wrapHsDriverOpen $ \oinfo -> do
    fname <- openInfoFilename oinfo
    eDs <- try $ do
      -- Make sure we identify it, gdal might still call us after
      -- identify returns false
      eValid <- hsdIdentify fname
      if not eValid then return HsdError else hsdOpen fname
    case eDs of
      Right (HsdDatasetH ptr)   -> return ptr
      Right (HsdDataset  ds)    -> toGDALDatasetIO ds
      Right HsdError            -> return nullDatasetH
      Left (e :: SomeException) -> do
        hPutStrLn stderr
          ("ERROR: Unhandled exception in HSDriver.open: " ++ show e)
        return nullDatasetH

  Driver <$> {#call unsafe hs_gdal_new_driver#} namePtr identify open

  where
    DriverName name = hsdName

withDriver :: (MonadIO m, MonadMask m) => HSDriver -> m a -> m a
withDriver info = bracket acquire deleteDriver . const
  where
    acquire = do
      d <- createDriver info
      registerDriver d
      return d

registerDriver :: MonadIO m => Driver t -> m ()
registerDriver =
  liftIO . void . {#call unsafe GDALRegisterDriver as ^#} . unDriver

deregisterDriver :: MonadIO m => Driver t -> m ()
deregisterDriver =
  liftIO . {#call unsafe GDALDeregisterDriver as ^#} . unDriver


deleteDriver :: MonadIO m => Driver t -> m ()
deleteDriver d = do
  -- first deregister if it is or bad things shall happen
  deregisterDriver d
  liftIO ({#call unsafe hs_gdal_delete_driver as ^#} (unDriver d))
  


openInfoFilename :: GDALOpenInfoH -> IO ByteString
openInfoFilename info = do
  s <- {#call unsafe hs_gdal_openinfo_filename#} info
  if s==nullPtr then return "" else packCString s
