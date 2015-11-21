{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GDAL.WarperSpec (main, spec) where

import Control.Monad (forM_)
import Data.Default (def)
import Data.Int (Int32)
import qualified Data.Vector.Unboxed as U

import GDAL
import OSR
import OGR (geomFromWkt)
import GDAL.Warper
import GDAL.Algorithms

import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "reprojectImage" $ do

    it "works with SpatialReferences in dataset" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326

      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection ds' srs1
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection ds2 srs2
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      reprojectImage ds Nothing ds2 Nothing 0 Nothing def

    it "does not work with no geotransforms" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection ds' srs1
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection ds2 srs2
      let action = reprojectImage ds Nothing ds2 Nothing 0 Nothing def
      action `shouldThrow` ((==AppDefined) . gdalErrNum)

    it "works with SpatialReferences as args" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      reprojectImage ds (Just srs1) ds2 (Just srs2) 0 Nothing def

    it "can be stopped with progressFun" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let pfun = Just (\_ _ -> return Stop)
          a = reprojectImage ds Nothing ds2 Nothing 0 pfun def
      a `shouldThrow` isInterruptedException

    it "can receive warp options" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def {woWarpOptions = [ ("OPTIMIZE_SIZE","TRUE") ]}
      reprojectImage ds Nothing ds2 Nothing 0 Nothing opts

    it "can receive cutline" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      let Right cl = geomFromWkt Nothing
                     "POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))"

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def {woCutline=Just cl}
      reprojectImage ds Nothing ds2 Nothing 0 Nothing opts

    it "cutline must be a polygon" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds' (Geotransform 0 10 0 0 0 (-10))
      ds <- unsafeToReadOnly ds'

      let Right cl = geomFromWkt Nothing "POINT (0 0)"
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform ds2 (Geotransform 0 10 0 0 0 (-10))
      let opts = def {woCutline=Just cl}
      reprojectImage ds Nothing ds2 Nothing 0 Nothing opts
         `shouldThrow` (==NonPolygonCutline)

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
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

        reprojectImage ds Nothing ds2 Nothing 0 Nothing def {woResampleAlg=algo}
        flushCache ds2

        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1

  describe "createWarpedVRT" $ do

    it "can receive cutline" $ do
      let gt = northUpGeotransform 100 (Envelope (-500) 500)
          Right cl = geomFromWkt Nothing
                     "POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))"
          opts = def {woCutline=Just cl}
          sz  = 100 :+: 100
          sz2 = 200 :+: 200
          v1 :: U.Vector (Value Int32)
          v1  = U.generate (sizeLen sz)
                (\i -> if i<50 then NoData else Value (fromIntegral i))
      ds' <- createMem sz 1 GDT_Int32 []
      setDatasetGeotransform ds' gt
      b <- getBand 1 ds'
      setBandNodataValue b ((-1) :: Int32)
      writeBand b (allBand b) sz v1
      ds <- unsafeToReadOnly ds'

      ds2 <- createWarpedVRT ds sz2 gt opts
      b2 <- getBand 1 ds2
      v2 <- readBand b2 (allBand b2) sz2
      v2 `shouldSatisfy` U.all (> Value 0)
      U.sum v2 `shouldSatisfy` (< U.sum v1)

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
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

        let opts = def { woResampleAlg = algo
                       , woTransfomer  =
                           SomeTransformer (def {giptSrcDs = Just ds})
                       }
        ds2 <- createWarpedVRT ds sz2 gt opts
        b2 <- getBand 1 ds2
        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer2) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
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

        let opts = def { woResampleAlg = algo
                       , woTransfomer  =
                           SomeTransformer (def {gipt2SrcDs = Just ds})}
        ds2 <- createWarpedVRT ds sz2 gt opts
        b2 <- getBand 1 ds2
        v2 <- readBand b2 (allBand b2) sz2
        v2 `shouldSatisfy` U.all (>(Value 0))
        U.sum v2 `shouldBe` U.sum v1


    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer3) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
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

        let opts = def { woResampleAlg = algo
                       , woTransfomer  =
                           SomeTransformer (def {gipt3SrcGt = Just gt})}
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
