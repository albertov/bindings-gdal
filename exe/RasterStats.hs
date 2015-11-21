{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Data.Int (Int16, Int32)

import GDAL

{-
type BandType = Int16
type SummaryType = Int

sumTypeInf, sumTypeNegInf :: SummaryType
sumTypeInf = maxBound
sumTypeNegInf = minBound

toDouble :: SummaryType -> Double
toDouble = fromIntegral

toSummaryType :: BandType -> SummaryType
toSummaryType = fromIntegral

-}

type SummaryType = Int
bandType = GDT_Int16
sumTypeInf, sumTypeNegInf :: SummaryType
sumTypeInf = maxBound --1/0
sumTypeNegInf = minBound --(-1/0)
toDouble :: SummaryType -> Double
toDouble = fromIntegral
--toSummaryType :: BandType -> SummaryType
toSummaryType = fromIntegral

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    openReadOnly fname bandType
      >>= getBand 1 >>= computeStatistics toSummaryType
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
  :: GDALType a
  => (a -> SummaryType) -> ROBand s a -> GDAL s Summary
computeStatistics toSummaryType'
  = fmap sumarize . GDAL.foldl' folder (Acc 0 0 sumTypeInf sumTypeNegInf 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
      where v = toSummaryType' v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = toDouble accS  / fromIntegral accCnt
        stddev = sqrt (toDouble accSq / fromIntegral accCnt - avg*avg)
