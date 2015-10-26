module GDAL (
    GDAL
  , GDALType (..)
  , DataType (..)
  , XY (..)
  , Window (..)
  , Size
  , BlockIx

  , GDALException (..)
  , GDALRasterException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException

  , Geotransform (..)
  , Driver (..)
  , Dataset
  , OptionList
  , ReadWrite
  , ReadOnly
  , RWDataset
  , RODataset
  , RWBand
  , ROBand
  , Band
  , Value (..)
  , ProgressFun
  , ContinueOrStop (..)

  , runGDAL
  , withGDAL
  , isNoData
  , fromValue
  , create
  , createMem
  , flushCache
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy

  , dataTypeSize
  , dataTypeByName
  , dataTypeUnion
  , dataTypeIsComplex
  , reifyDataType

  , datasetSize
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetBandCount

  , bandDataType
  , reifyBandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandSize
  , allBand
  , winSize
  , sizeLen
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , readBand
  , readBandPure
  , readBandBlock
  , writeBand
  , writeBandBlock

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'
  , version
) where

import Control.Exception (finally)

import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.GDAL as GDAL
import GDAL.Internal.Types

import qualified GDAL.Internal.OGR as OGR
import qualified GDAL.Internal.OSR as OSR

-- | Performs process-wide initialization and cleanup
--   Should only be called from the main thread
withGDAL :: IO a -> IO a
withGDAL a =
    (GDAL.allRegister >> OGR.registerAll >> a)
      `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager >> OSR.cleanup)
