module GDAL (
    GDAL
  , GDALType
  , ApproxOK (..)
  , DataType
  , gdtByte
  , gdtUInt16
  , gdtUInt32
  , gdtInt16
  , gdtInt32
  , gdtFloat32
  , gdtFloat64
  , gdtCInt16
  , gdtCInt32
  , gdtCFloat32
  , gdtCFloat64
  , gdtUnknown
  , XY (..)
  , Size
  , BlockIx

  , GDALException (..)
  , GDALRasterException (..)
  , ProgressException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException
  , isProgressFunException
  , isInterruptedException

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
  , MaskType (MaskPerBand, MaskPerDataset)
  , Value (..)
  , ProgressFun
  , ContinueOrStop (..)

  , dataType
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

  , bandTypedAs
  , bandCoercedTo

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
  , createBandMask
  , readBandBlock
  , writeBand
  , writeBandBlock
  , copyBand
  , fillBand
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
import GDAL.Internal.Types.Value
import GDAL.Internal.Common

import qualified GDAL.Internal.OGR as OGR
import qualified GDAL.Internal.OSR as OSR

-- | Performs process-wide initialization and cleanup
--   Should only be called from the main thread
withGDAL :: IO a -> IO a
withGDAL a =
    (GDAL.allRegister >> OGR.registerAll >> OSR.initialize >> a)
      `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager >> OSR.cleanup)
