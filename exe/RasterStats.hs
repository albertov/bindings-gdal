{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Data.Int (Int16)

import GDAL

type BandType = Int16
type SummaryType = Int

sumTypeInf, sumTypeNegInf :: SummaryType
sumTypeInf = maxBound
sumTypeNegInf = minBound

toDouble :: SummaryType -> Double
toDouble = fromIntegral

toSummaryType :: BandType -> SummaryType
toSummaryType = fromIntegral

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    b <- openReadOnly fname >>= getBand 1
    computeStatistics b
  print summary

data Acc = Acc
  { accS   :: {-# UNPACK #-} !SummaryType
  , accSq  :: {-# UNPACK #-} !SummaryType
  , accMin :: {-# UNPACK #-} !SummaryType
  , accMax :: {-# UNPACK #-} !SummaryType
  , accCnt :: {-# UNPACK #-} !Int
  }

type Summary = (Double, Double, SummaryType, SummaryType)

computeStatistics
  :: ROBand s BandType -> GDAL s Summary
computeStatistics
  = fmap sumarize . GDAL.foldl' folder (Acc 0 0 sumTypeInf sumTypeNegInf 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
      where v = toSummaryType v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = toDouble accS  / fromIntegral accCnt
        stddev = sqrt (toDouble accSq / fromIntegral accCnt - avg*avg)
