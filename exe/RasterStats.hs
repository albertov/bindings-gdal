{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Data.Int (Int64, Int16)
import qualified Data.Vector.Generic as G

import GDAL

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    b <- openReadOnly fname >>= getBand 1
    computeStatistics (b `bandTypedAs` (undefined::Int16))
  print summary

data Acc = Acc
  { accS   :: {-# UNPACK #-} !Int64
  , accSq  :: {-# UNPACK #-} !Int64
  , accMin :: {-# UNPACK #-} !Int64
  , accMax :: {-# UNPACK #-} !Int64
  , accCnt :: {-# UNPACK #-} !Int64
  }

type Summary = (Double, Double, Int64, Int64)

computeStatistics
  :: (Integral a, GDALType a)
  => ROBand s a -> GDAL s Summary
computeStatistics
  = fmap sumarize . GDAL.foldl' folder (Acc 0 0 maxBound minBound 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt + 1)
      where v = fromIntegral v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg :: Double
        avg    = fromIntegral accS  / fromIntegral accCnt
        stddev = sqrt (fromIntegral accSq / fromIntegral accCnt - avg*avg)
