{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module GDAL.AlgorithmsSpec (main, spec) where

import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)

import OGR
import OSR
import GDAL
import GDAL.Algorithms

import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "rasterizeLayersBuf" $ do

   it "produces a NoData vector when rasterizing an empty layer" $ do
     ds <- OGR.createMem []
     let fDef = featureDef (Proxy :: Proxy (TestFeature Double Double))
         size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
     layer <- liftM unsafeToReadOnlyLayer $
                 createLayerWithDef ds fDef StrictOK []
     v <- rasterizeLayersBuf
            [layer]
            (0 :: Double)
            (Left 1)
            srs4326
            size
            (northUpGeotransform size env)
            def
     v `shouldSatisfy` U.all (==NoData)

   it "burns value passed as parameter" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 87 99
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- rasterizeLayersBuf
            [unsafeToReadOnlyLayer l]
            (0 :: Double)
            (Left burnValue)
            srs4326
            size
            (northUpGeotransform size env)
            def
     v `shouldSatisfy` U.any (==(Value burnValue))
     v `shouldSatisfy` U.all (/=(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

   it "burns attribute from feature" $ do
     ds <- OGR.createMem []
     let size = 100
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 15 0
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- rasterizeLayersBuf
            [unsafeToReadOnlyLayer l]
            (0 :: Double)
            (Right "field1")
            srs4326
            size
            (northUpGeotransform size env)
            def
     v `shouldSatisfy` U.any (==(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

   it "transforms geometries" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 7 9
         Right ct = coordinateTransformation srs4326 srs23030
         env4326  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         Just env23030 = env4326 `transformWith` ct
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- rasterizeLayersBuf
            [unsafeToReadOnlyLayer l]
            (0 :: Double)
            (Left burnValue)
            srs23030
            size
            (northUpGeotransform size env4326)
            def
     v `shouldSatisfy` U.all (==NoData)
     v `shouldSatisfy` U.all (/=(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

     w <- rasterizeLayersBuf
            [unsafeToReadOnlyLayer l]
            (0 :: Double)
            (Left burnValue)
            srs23030
            size
            (northUpGeotransform size env23030)
            def
     w `shouldSatisfy` U.any (==(Value burnValue))

  describe "rasterizeLayers" $ do

   it "produces a NoData vector when rasterizing an empty layer" $ do
     ds <- OGR.createMem []
     let fDef = featureDef (Proxy :: Proxy (TestFeature Double Double))
         size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         gt   = northUpGeotransform size env
     layer <- liftM unsafeToReadOnlyLayer $
                 createLayerWithDef ds fDef StrictOK []
     dDs <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gt dDs
     b <- getBand 1 dDs
     setBandNodataValue 0 b
     rasterizeLayers (Left [(layer,1)]) dDs def
     readBand b (allBand b) (bandSize b) >>= (`shouldSatisfy` U.all (==NoData))

   it "burns value passed as parameter" $ do
     ds <- OGR.createMem []
     let size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         gt   = northUpGeotransform size env
         burnValue = 10
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 87 99
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l
     dDs <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gt dDs
     b <- getBand 1 dDs
     setBandNodataValue 0 b
     rasterizeLayers (Left [(unsafeToReadOnlyLayer l, burnValue)]) dDs def
     v <- readBand b (allBand b) (bandSize b)
     v `shouldSatisfy` U.any (==(Value burnValue))
     v `shouldSatisfy` U.all (/=(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

   it "burns attribute from feature" $ do
     ds <- OGR.createMem []
     let size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         gt   = northUpGeotransform size env
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 15 0
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l
     dDs <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gt dDs
     b <- getBand 1 dDs
     setBandNodataValue 0 b
     rasterizeLayers (Right ([unsafeToReadOnlyLayer l], "field1")) dDs def
     v <- readBand b (allBand b) (bandSize b)
     v `shouldSatisfy` U.any (==(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

   it "transforms geometries" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom 7 9
         Right ct = coordinateTransformation srs4326 srs23030
         env4326  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         Just env23030 = env4326 `transformWith` ct
         gtV = northUpGeotransform size env4326
         gtW = northUpGeotransform size env23030
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     dDsV <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gtV dDsV
     setDatasetProjection srs23030 dDsV
     bV <- getBand 1 dDsV
     setBandNodataValue 0 bV
     rasterizeLayers (Left [(unsafeToReadOnlyLayer l, burnValue)]) dDsV def
     v <- readBand bV (allBand bV) (bandSize bV)

     v `shouldSatisfy` U.all (==NoData)
     v `shouldSatisfy` U.all (/=(Value (tfField1 feat)))
     v `shouldSatisfy` U.all (/=(Value (tfField2 feat)))

     dDsW <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gtW dDsW
     setDatasetProjection srs23030 dDsW
     bW <- getBand 1 dDsW
     setBandNodataValue 0 bW
     rasterizeLayers (Left [(unsafeToReadOnlyLayer l, burnValue)]) dDsW def
     w <- readBand bW (allBand bW) (bandSize bW)

     w `shouldSatisfy` U.any (==(Value burnValue))

  describe "rasterizeGeometries" $ do

   it "produces a NoData vector when rasterizing an empty geom list " $ do
     let size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         gt   = northUpGeotransform size env
     dDs <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gt dDs
     b <- getBand 1 dDs
     setBandNodataValue 0 b
     rasterizeGeometries [] dDs def
     readBand b (allBand b) (bandSize b) >>= (`shouldSatisfy` U.all (==NoData))

   it "burns values associated with geometry" $ do
     let size = 100
         env  = Envelope ((-3) :+: 42) ((-2) :+: 43)
         gt   = northUpGeotransform size env
         burnValue = 10
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
     dDs <- GDAL.createMem size 1 GDT_Float64 []
     setDatasetGeotransform gt dDs
     b <- getBand 1 dDs
     setBandNodataValue 0 b
     rasterizeGeometries [(geom,[burnValue])] dDs def
     v <- readBand b (allBand b) (bandSize b)
     v `shouldSatisfy` U.any (==(Value burnValue))

   it "checks that value list length matches band list length" $ do
     dDs <- GDAL.createMem 1 1 GDT_Float64 []
     rasterizeGeometries [(undefined,[])] dDs def
       `shouldThrow` (==GeomValueListLengthMismatch)


  createGridIOSpec GDT_Float64 (SGA (def :: GridInverseDistanceToAPower))
  createGridIOSpec GDT_Float64 (SGA (def :: GridMovingAverage))
  createGridIOSpec GDT_Float64 (SGA (def :: GridNearestNeighbor))

  createGridIOSpec GDT_Int32 (SGA (def :: GridInverseDistanceToAPower))
  createGridIOSpec GDT_Int32 (SGA (def :: GridMovingAverage))
  createGridIOSpec GDT_Int32 (SGA (def :: GridNearestNeighbor))

  -- GDAL discards the imaginary part so these don't compile
  --createGridIOSpec GDT_CFloat64 (SGA (def :: GridInverseDistanceToAPower))
  --createGridIOSpec GDT_CFloat64 (SGA (def :: GridMovingAverage))
  --createGridIOSpec GDT_CFloat64 (SGA (def :: GridNearestNeighbor))
  --createGridIOMetricsSpec GDT_CFloat64

  createGridIOMetricsSpec GDT_Float64
  createGridIOMetricsSpec GDT_Int32


  describe "contourGenerateVectorIO" $ do

    itIO "produces no contours if vector is all nodata" $ do
      let vec    = St.replicate (sizeLen sz) noData
          noData = 0
          sz     = 100
      contours <- contourGenerateVectorIO 10 0 (Just noData) sz vec
      length contours `shouldBe` 0

    itIO "produces no contours if vector is uniform and no nodata" $ do
      let vec    = St.replicate (sizeLen sz) val
          val    = 3
          sz     = 100
      contours <- contourGenerateVectorIO 10 0 Nothing sz vec
      length contours `shouldBe` 0

    itIO "produces contours if vector has data" $ do
      let vec    = St.generate (sizeLen sz) (distance center . toPair)
          noData = 0
          center = fmap fromIntegral (div <$> sz <*> 2)
          toPair   = fmap fromIntegral . uncurry (:+:) . flip divMod (pFst sz)
          sz     = 100
          distance (x :+: y) (x' :+: y') = sqrt (sq (x-x') + sq (y-y'))
          sq     = (^ (2::Int))
      contours <- contourGenerateVectorIO 10 0 (Just noData) sz vec
      length contours `shouldBe` 13
      sum (map cLevel contours) `shouldBe` 650
      sum (map (St.length . cPoints) contours) `shouldBe` 1300
      minimum (map (St.minimum . St.map pFst . cPoints) contours)
        `shouldSatisfy` (>=0)
      minimum (map (St.minimum . St.map pSnd . cPoints) contours)
        `shouldSatisfy` (>=0)
      maximum (map (St.maximum . St.map pFst . cPoints) contours)
        `shouldSatisfy` (<=fromIntegral (pFst sz))
      maximum (map (St.maximum . St.map pSnd . cPoints) contours)
        `shouldSatisfy` (<=fromIntegral (pSnd sz))


describeWith :: Show a => a -> (a -> SpecWith ()) -> SpecWith ()
describeWith opts act = describe ("with "++ show opts) (act opts)

data SomeGridAlgorithm = forall a. GridAlgorithm a => SGA a


createGridIOSpec
  :: (GDALType (HsType d), IsComplex d ~ 'False)
  => DataType d -> SomeGridAlgorithm -> SpecWith (Arg (IO ()))
createGridIOSpec dt (SGA opts) = do

  describe ("createGridIO " ++ show dt ++ "(with "++ show opts ++")") $ do

    itIO "produces a NoData vector when no points" $ do
      vec <- createGridIO
               dt
               opts
               (-1)
               Nothing
               []
               (Envelope (-500) 500)
               (100 :+: 100)
      vec `shouldSatisfy` U.all (==NoData)

    itIO "produces a vector with values when some points" $ do
      vec <- createGridIO
               dt
               opts
               (-1)
               Nothing
               [GP (0 :+: 0) 10]
               (Envelope (-500) 500)
               (100 :+: 100)
      vec `shouldSatisfy` U.any (/=NoData)


createGridIOMetricsSpec
  :: (Ord (HsType d), GDALType (HsType d), IsComplex d ~ 'False)
  => DataType d -> SpecWith (Arg (IO ()))
createGridIOMetricsSpec dt = do
  describe ("createGridIO " ++ show dt ++ " (GridDataMetrics)") $ do

    describeWith (def {dmType = MetricCount}) $ \opts -> do

      itIO "produces all zeros when no points" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.all (==Value 0)

      itIO "produces a vector with some values > 0" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 [GP (0 :+: 0) 10]
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.any (>Value 0)


    describeWith (def {dmType = MetricMaximum}) $ \opts -> do

      itIO "produces a NoData vector when no points" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.all (==NoData)

      itIO "produces a vector that contains the maximum value" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 [ GP (2 :+: 2) 10
                 , GP (0 :+: 0) 2]
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.any (==Value 10)
        vec `shouldSatisfy` U.all (/=Value 2)

    describeWith (def {dmType = MetricMinimum}) $ \opts -> do

      itIO "produces a NoData vector when no points" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.all (==NoData)

      itIO "produces a vector that contains the minimum value" $ do
        vec <- createGridIO
                 dt
                 opts
                 (-1)
                 Nothing
                 [ GP (2 :+: 2) 10
                 , GP (0 :+: 0) 2]
                 (Envelope (-500) 500)
                 (100 :+: 100)
        vec `shouldSatisfy` U.any (==Value 2)
        vec `shouldSatisfy` U.all (/=Value 10)

liftMaybe :: Either b a -> Maybe a
liftMaybe = either (const Nothing) Just

srs23030 :: SpatialReference
srs23030 = either exc id (srsFromEPSG 23030)
  where exc = error . ("Unexpected srsFromEPSG error: " ++) . show

srs4326 :: SpatialReference
srs4326 = either exc id (srsFromEPSG 4326)
  where exc = error . ("Unexpected srsFromEPSG error: " ++) . show

data TestFeature a b
  = TestFeature  {
      tfGeom :: Geometry
    , tfField1 :: a
    , tfField2 :: b
  } deriving (Eq, Show)

instance (OGRField a, OGRField b) => OGRFeature (TestFeature a b) where
  toFeature TestFeature{..} = feature tfGeom [ "field1" .= tfField1
                                             , "field2" .= tfField2]
  fromFeature f             = TestFeature <$> theGeom f
                                          <*> f .: "field1"
                                          <*> f .: "field2"


instance (OGRField a, OGRField b) => OGRFeatureDef (TestFeature a b) where
  featureDef _ =
    FeatureDef {
      fdName   = "Test"
    , fdFields = [ "field1" `fieldTypedAs` (undefined :: a)
                 , "field2" `fieldTypedAs` (undefined :: b)
                 ]
    , fdGeom   = GeomFieldDef WkbPolygon (Just srs4326) False
    , fdGeoms  = mempty}
