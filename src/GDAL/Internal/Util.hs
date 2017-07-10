{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.Util (
    fromEnumC
  , toEnumC
  , runBounded
) where

import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Foreign.C.Types (CInt)

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral

runBounded :: MonadBaseControl IO m => m a -> m a
runBounded
  | rtsSupportsBoundThreads = liftBaseOp_ runInBoundThread
  | otherwise               = id
