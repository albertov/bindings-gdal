module GDAL (
    GDAL
  , GDALType
  , Datatype (..)
  , GDALException (..)
  , ErrorType (..)
  , isGDALException
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

  , runGDAL
  , gdalForkIO
  , isNoData
  , fromValue
  , registerAllDrivers
  , destroyDriverManager
  , setQuietErrorHandler
  , create
  , createMem
  , flushCache
  , openReadOnly
  , openReadWrite
  , createCopy

  , datatypeSize
  , datatypeByName
  , datatypeUnion
  , datatypeIsComplex

  , datasetSize
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetBandCount

  , bandDatatype
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandSize
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , readBand
  , readBandPure
  , readBandBlock
  , writeBand
  , writeBandBlock
  , fillBand

  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'
) where

import GDAL.Internal.GDAL
import GDAL.Internal.Types
import GDAL.Internal.CPLString
