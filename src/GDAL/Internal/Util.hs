{-# LANGUAGE FlexibleContexts #-}
module GDAL.Internal.Util (
    fromEnumC
  , toEnumC
  , runBounded
) where

import Control.Concurrent (rtsSupportsBoundThreads)
import Foreign.C.Types (CInt)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Concurrent (runInBoundThread)

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral

runBounded :: MonadUnliftIO m => m a -> m a
runBounded
  | rtsSupportsBoundThreads = runInBoundThread
  | otherwise               = id
