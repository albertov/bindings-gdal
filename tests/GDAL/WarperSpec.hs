{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.WarperSpec (main, spec) where

import Control.Monad (void)
import Data.Default (def)
import Data.Int (Int16)

import Test.Hspec (Spec, SpecWith, hspec, describe)

import System.FilePath (joinPath)

import GDAL
import GDAL.OSR
import GDAL.Warper
import GDAL.Algorithms

import Paths_bindings_gdal

import TestUtils (
    shouldThrow
  , shouldBe
  , shouldNotBe
  , shouldContain
  , shouldSatisfy
  , it
  , withDir
  )

import GDALSpec (setupAndTeardown)

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "reprojectImage" $ do

    it "works with SpatialReferences in dataset" $ do
      let Right srs1 = fromEPSG 23030
          Right srs2 = fromEPSG 4326

      ds' <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetProjection ds' srs1
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetProjection ds2 srs2
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      reprojectImage ds ds2 Nothing

    it "does not work with no geotransforms" $ do
      let Right srs1 = fromEPSG 23030
          Right srs2 = fromEPSG 4326
      ds' <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetProjection ds' srs1
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetProjection ds2 srs2
      reprojectImage ds ds2 Nothing `shouldThrow` ((==AppDefined) . gdalErrNum)

    it "works with SpatialReferences as args to transformer" $ do
      let Right srs1 = fromEPSG 23030
          Right srs2 = fromEPSG 4326
      ds' <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def `setTransformer` def { giptSrcSrs = Just srs1
                                          , giptDstSrs = Just srs2}
      reprojectImage ds ds2 (Just opts)

    it "can be stopped with progressFun" $ do
      ds' <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def {woProgressFun = Just (\_ _ -> return Stop)}
      reprojectImage ds ds2 (Just opts) `shouldThrow` (==WarpStopped)

    it "can receive warp options" $ do
      ds' <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def {woWarpOptions = [("OPTIMIZE_SIZE","TRUE")]}
      reprojectImage ds ds2 (Just opts)
