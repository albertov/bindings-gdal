{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module GDAL.AlgorithmsSpec (main, spec) where

import Data.Default (def)
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, sequence)

import OGR
import OSR
import GDAL
import GDAL.Algorithms

import TestUtils
import GDALSpec (setupAndTeardown)

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "rasterizeLayersBuf" $ do

   it "produces a NoData vector when rasterizing an empty layer" $ do
     ds <- OGR.createMem []
     let fDef = featureDef (Proxy :: Proxy (TestFeature Double Double))
         size = 100
         env  = Envelope (XY (-3) 42) (XY (-2) 43)
         mkLayer = liftM unsafeToReadOnlyLayer $
                     createLayerWithDef ds fDef StrictOK []
     v <- runOGR $
          rasterizeLayersBuf
            (sequence [mkLayer])
            DefaultTransformer
            0
            1
            []
            Nothing
            srs4326
            size
            (northUpGeotransform size env)
     (v :: U.Vector (Value Double)) `shouldSatisfy` U.all (==NoData)

   it "burns value passed as parameter" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10 :: Double
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom (0 :: Double) (0 :: Double)
         env  = Envelope (XY (-3) 42) (XY (-2) 43)
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- runOGR $
          rasterizeLayersBuf
            (sequence [liftM unsafeToReadOnlyLayer (getLayer 0 ds)])
            DefaultTransformer
            0
            burnValue
            []
            Nothing
            srs4326
            size
            (northUpGeotransform size env)
     v `shouldSatisfy` U.any (==(Value burnValue))

   it "burns attribute from feature" $ do
     ds <- OGR.createMem []
     let size = 100
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom (15 :: Double) (0 :: Double)
         env  = Envelope (XY (-3) 42) (XY (-2) 43)
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- runOGR $
          rasterizeLayersBuf
            (sequence [liftM unsafeToReadOnlyLayer (getLayer 0 ds)])
            DefaultTransformer
            0
            0
            [("attribute","field1")]
            Nothing
            srs4326
            size
            (northUpGeotransform size env)
     v `shouldSatisfy` U.any (==(Value (tfField1 feat)))

   it "transforms geometries" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10 :: Double
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs4326) "POINT (-2.5 42.5)")
           geomBuffer 0.05 10 g
         feat = TestFeature geom (0 :: Double) (0 :: Double)
         Right ct = coordinateTransformation srs4326 srs23030
         env4326  = Envelope (XY (-3) 42) (XY (-2) 43)
         Just env23030 = env4326 `transformWith` ct
     l <- createLayer ds StrictOK []
     createFeature_ l feat
     syncLayerToDisk l

     v <- runOGR $
          rasterizeLayersBuf
            (sequence [liftM unsafeToReadOnlyLayer (getLayer 0 ds)])
            DefaultTransformer
            0
            burnValue
            []
            Nothing
            srs23030
            size
            (northUpGeotransform size env4326)
     v `shouldSatisfy` U.all (==NoData)

     w <- runOGR $
          rasterizeLayersBuf
            (sequence [liftM unsafeToReadOnlyLayer (getLayer 0 ds)])
            DefaultTransformer
            0
            burnValue
            []
            Nothing
            srs23030
            size
            (northUpGeotransform size env23030)
     w `shouldSatisfy` U.any (==(Value burnValue))


  createGridIOSpec (SGA (def :: GridInverseDistanceToAPower))
  createGridIOSpec (SGA (def :: GridMovingAverage))
  createGridIOSpec (SGA (def :: GridNearestNeighbor))

  describe "createGridIO (GridDataMetrics)" $ do

    describeWith (def {dmType = MetricCount}) $ \opts -> do

      itIO "produces all zeros when no points" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.all (==(Value 0 :: Value Double))

      itIO "produces a vector with some values > 0" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 [GP (XY 0 0) 10]
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.any (>(Value 0 :: Value Double))


    describeWith (def {dmType = MetricMaximum}) $ \opts -> do

      itIO "produces a NoData vector when no points" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.all (==(NoData :: Value Double))

      itIO "produces a vector that contains the maximum value" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 [ GP (XY 2 2) 10
                 , GP (XY 0 0) 2]
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.any (==(Value 10 :: Value Double))
        vec `shouldSatisfy` U.all (/=(Value 2 :: Value Double))

    describeWith (def {dmType = MetricMinimum}) $ \opts -> do

      itIO "produces a NoData vector when no points" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 []
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.all (==(NoData :: Value Double))

      itIO "produces a vector that contains the minimum value" $ do
        vec <- createGridIO
                 opts
                 (-1)
                 Nothing
                 [ GP (XY 2 2) 10
                 , GP (XY 0 0) 2]
                 (Envelope (-500) 500)
                 (XY 100 100)
        vec `shouldSatisfy` U.any (==(Value 2 :: Value Double))
        vec `shouldSatisfy` U.all (/=(Value 10 :: Value Double))

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
      let vec    = St.generate (sizeLen sz) (distance center . toXY)
          noData = 0
          center = fmap fromIntegral (div <$> sz <*> 2)
          toXY   = fmap fromIntegral . uncurry XY . flip divMod (px sz)
          sz     = 100
          distance (XY x y) (XY x' y') = sqrt (sq (x-x') + sq (y-y'))
          sq     = (^ (2::Int))
      contours <- contourGenerateVectorIO 10 0 (Just noData) sz vec
      length contours `shouldBe` 8


describeWith :: Show a => a -> (a -> SpecWith ()) -> SpecWith ()
describeWith opts act = describe ("with "++ show opts) (act opts)

data SomeGridAlgorithm = forall a. GridAlgorithm a => SGA a


createGridIOSpec :: SomeGridAlgorithm -> SpecWith (Arg (IO ()))
createGridIOSpec (SGA opts) = do

  describe ("createGridIO (with "++ show opts ++")") $ do

    itIO "produces a NoData vector when no points" $ do
      vec <- createGridIO
               opts
               (-1)
               Nothing
               []
               (Envelope (-500) 500)
               (XY 100 100)
      vec `shouldSatisfy` U.all (==(NoData :: Value Double))

    itIO "produces a vector with values when some points" $ do
      vec <- createGridIO
               opts
               (-1)
               Nothing
               [GP (XY 0 0) 10]
               (Envelope (-500) 500)
               (XY 100 100)
      vec `shouldSatisfy` U.any (/=(NoData :: Value Double))



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
