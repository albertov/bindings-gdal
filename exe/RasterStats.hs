{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Data.Int (Int64, Int16)

import GDAL

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    b <- openReadOnly fname >>= getBand 1
    computeStatistics fromIntegral (b `bandTypedAs` (undefined::Int16))
  print summary

data Acc = Acc
  { accS   :: {-# UNPACK #-} !Double
  , accSq  :: {-# UNPACK #-} !Double
  , accMin :: {-# UNPACK #-} !Double
  , accMax :: {-# UNPACK #-} !Double
  , accCnt :: {-# UNPACK #-} !Int64
  }

type Summary = (Double, Double, Double, Double)

computeStatistics
  :: GDALType a => (a -> Double) -> ROBand s a -> GDAL s Summary
computeStatistics f
  = fmap sumarize . GDAL.foldl' folder (Acc 0 0 (1/0) (-1/0) 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
      where v = f v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = accS  / fromIntegral accCnt
        stddev = sqrt (accSq / fromIntegral accCnt - avg*avg)
