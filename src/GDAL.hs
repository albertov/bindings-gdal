{-# LANGUAGE CPP #-}
module GDAL (
    GDAL
  , GDALType
  , ApproxOK (..)
  , DataType (..)
  , HsType
  , TypeK
  , IsComplex
  , Pair (..)
  , pFst
  , pSnd
  , Envelope (..)
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
  , Driver
  , DriverName
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
  , HasProgressFun (..)
  , HasOptions (..)
  , ContinueOrStop (..)

  , dataType
  , runGDAL
  , runGDAL_
  , withGDAL
  , initializeGDAL
  , cleanupGDAL
  , isNoData
  , fromValue
  , unValueVector
  , catValues

  , driverByName
  , registerDriver
  , deregisterDriver
  , deleteDriver
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

  , bandAs

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
  , bandConduit
  , unsafeBandConduit
  , bandSink
  , bandSinkGeo
  , fillBand
  , fmapBand
  , blockConduit
  , unsafeBlockConduit
  , blockSink
  , allBlocks
  , blockSource
  , unsafeBlockSource
  , zipBlocks
  , getZipBlocks


  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , gcp

  , foldl'
  , ifoldl'
  , foldlWindow'
  , ifoldlWindow'
  , version

  , gcpGeotransform
  , northUpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)
  , geoEnvelopeTransformer

  , liftIO
  , withConfigOption
  , setConfigOption
  , getConfigOption
  , module Data.Conduit
  , def
  , ensureDataExists
) where

import Control.Exception (bracket_)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Conduit

import GDAL.Internal.CPLConv
import GDAL.Internal.CPLError
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.GCP
import GDAL.Internal.GDAL as GDAL
import GDAL.Internal.Types
import GDAL.Internal.Types.Value
import GDAL.Internal.Common
import OGR (Envelope(..))

import qualified GDAL.Internal.OGR as OGR
import qualified GDAL.Internal.OSR as OSR

#if HAVE_EMBEDDED_DATA
import qualified GDAL.Internal.GDALData as GD
ensureDataExists :: IO ()
ensureDataExists = GD.ensureExists
#else
import System.IO
ensureDataExists :: IO ()
ensureDataExists =
  hPutStrLn stderr
  "WARNING: bindingsr-gdal have not been compiled with embeded GDAL_DATA" 
#endif

-- | Performs process-wide initialization and cleanup
--   Should only be called from the main thread
withGDAL :: IO a -> IO a
withGDAL = bracket_ initializeGDAL cleanupGDAL

initializeGDAL, cleanupGDAL :: IO ()
initializeGDAL = do
#if HAVE_EMBEDDED_DATA
  ensureDataExists
#endif
  GDAL.allRegister >> OGR.registerAll >> OSR.initialize

cleanupGDAL    = OGR.cleanupAll >> GDAL.destroyDriverManager >> OSR.cleanup
