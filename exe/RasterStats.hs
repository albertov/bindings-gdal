{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)

import GDAL

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    b <- openReadOnly fname >>= getBand 1
    computeStatistics b
  print summary

data Acc = Acc
  { accS   :: {-# UNPACK #-} !Double
  , accSq  :: {-# UNPACK #-} !Double
  , accMin :: {-# UNPACK #-} !Double
  , accMax :: {-# UNPACK #-} !Double
  , accCnt :: {-# UNPACK #-} !Int
  }

type Summary = (Double, Double, Double, Double)

computeStatistics
  :: ROBand s Double -> GDAL s Summary
computeStatistics
  = fmap sumarize . GDAL.foldl' folder (Acc 0 0 (1/0) (-1/0) 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
      where v = v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = accS  / fromIntegral accCnt
        stddev = sqrt (accSq / fromIntegral accCnt - avg*avg)
