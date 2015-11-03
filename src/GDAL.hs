module GDAL (
    GDAL
  , GDALType (..)
  , ApproxOK (..)
  , DataType (..)
  , XY (..)
  , Size
  , BlockIx

  , GDALException (..)
  , GDALRasterException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException

  , Geotransform (..)
  , GroundControlPoint (..)
  , OverviewResampling (..)
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
  , execGDAL
  , withGDAL
  , isNoData
  , fromValue
  , create
  , createMem
  , delete
  , rename
  , copyFiles
  , flushCache
  , openReadOnly
  , openReadWrite
  , closeDataset
  , unsafeToReadOnly
  , createCopy
  , buildOverviews

  , dataTypeSize
  , dataTypeByName
  , dataTypeUnion
  , dataTypeIsComplex
  , reifyDataType

  , datasetDriver
  , datasetSize
  , datasetFileList
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetGCPs
  , setDatasetGCPs
  , datasetBandCount

  , bandDataType
  , reifyBandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandSize
  , bandHasOverviews
  , allBand
  , sizeLen
  , bandNodataValue
  , setBandNodataValue
  , addBand
  , getBand
  , readBand
  , readBandBlock
  , writeBand
  , writeBandBlock
  , copyBand
  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , gcp

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'
  , version

  , gcpGeotransform
  , northUpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)
) where

import Control.Exception (finally)

import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.GCP
import GDAL.Internal.GDAL as GDAL
import GDAL.Internal.Types
import GDAL.Internal.Common

import qualified GDAL.Internal.OGR as OGR
import qualified GDAL.Internal.OSR as OSR

-- | Performs process-wide initialization and cleanup
--   Should only be called from the main thread
withGDAL :: IO a -> IO a
withGDAL a =
    (GDAL.allRegister >> OGR.registerAll >> OSR.initialize >> a)
      `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager >> OSR.cleanup)
