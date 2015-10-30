{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GDAL.WarperSpec (main, spec) where

import Control.Monad (void, forM_)
import Data.Default (def)
import Data.Int (Int16, Int32)
import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)

import GDAL
import OSR
import GDAL.Warper
import GDAL.Algorithms

import Paths_bindings_gdal

import TestUtils

import GDALSpec (setupAndTeardown)

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "reprojectImage" $ do

    it "works with SpatialReferences in dataset" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326

      ds' <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetProjection ds' srs1
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetProjection ds2 srs2
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      reprojectImage ds Nothing ds2 Nothing NearestNeighbour 0 0 Nothing []

    it "does not work with no geotransforms" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetProjection ds' srs1
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetProjection ds2 srs2
      let action = reprojectImage ds Nothing ds2 Nothing NearestNeighbour 0 0
                     Nothing []
      action `shouldThrow` ((==AppDefined) . gdalErrNum)

    it "works with SpatialReferences as args" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      reprojectImage ds (Just srs1) ds2 (Just srs2) NearestNeighbour 0 0
        Nothing []

    it "can be stopped with progressFun" $ do
      ds' <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let pfun = Just (\_ _ -> return Stop)
          a = reprojectImage ds Nothing ds2 Nothing NearestNeighbour 0 0 pfun []
      a `shouldThrow` (==WarpStopped)

    it "can receive warp options" $ do
      ds' <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (XY 100 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = [ ("OPTIMIZE_SIZE","TRUE") ]
      reprojectImage ds Nothing ds2 Nothing NearestNeighbour 0 0 Nothing opts

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata " ++ show algo) $ do
        let sz  = XY 100 100
            sz2 = XY 200 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1 :: U.Vector (Value Int32)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform ds' gt
        b <- getBand 1 ds'
        setBandNodataValue b ((-1) :: Int32)
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        ds2 <- createMem sz2 1 GDT_Int32 []
        setDatasetGeotransform ds2 gt
        b2 <- getBand 1 ds2
        setBandNodataValue b2 ((-2) :: Int32)

        reprojectImage ds Nothing ds2 Nothing algo 0 0 Nothing []
        flushCache ds2

        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1

  describe "createWarpedVRT" $ do

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer) " ++ show algo) $ do
        let sz  = XY 100 100
            sz2 = XY 200 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1 :: U.Vector (Value Int32)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform ds' gt
        b <- getBand 1 ds'
        setBandNodataValue b ((-1) :: Int32)
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = setTransformer (def {giptSrcDs = Just ds})
                                  (def {woResampleAlg = algo})
        ds2 <- createWarpedVRT ds sz2 gt opts
        b2 <- getBand 1 ds2
        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer2) " ++ show algo) $ do
        let sz  = XY 100 100
            sz2 = XY 200 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1 :: U.Vector (Value Int32)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform ds' gt
        b <- getBand 1 ds'
        setBandNodataValue b ((-1) :: Int32)
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = setTransformer (def {gipt2SrcDs = Just ds})
                                  (def {woResampleAlg = algo})

        ds2 <- createWarpedVRT ds sz2 gt opts
        b2 <- getBand 1 ds2
        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1


    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer3) " ++ show algo) $ do
        let sz  = XY 100 100
            sz2 = XY 200 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1 :: U.Vector (Value Int32)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform ds' gt
        b <- getBand 1 ds'
        setBandNodataValue b ((-1) :: Int32)
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = setTransformer (def {gipt3SrcGt = Just gt})
                                  (def {woResampleAlg = algo})

        ds2 <- createWarpedVRT ds sz2 gt opts
        b2 <- getBand 1 ds2
        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1

resampleAlgorithmsWhichHandleNodata :: [ResampleAlg]
resampleAlgorithmsWhichHandleNodata
  = filter (`notElem` bad) [minBound..maxBound]
  where
    bad
      | GDAL.version >= (1, 11) = [CubicSpline]
      | otherwise               = [CubicSpline, Mode, Average]
