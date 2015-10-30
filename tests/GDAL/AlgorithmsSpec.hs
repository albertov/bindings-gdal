{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module GDAL.AlgorithmsSpec (main, spec) where

import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Unboxed as U

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
         env  = Envelope (-500) 500
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
            srs23030
            size
            (northUpGeotransform size env)
     (v :: U.Vector (Value Double)) `shouldSatisfy` U.all (==NoData)

   it "burns value passed as parameter" $ do
     ds <- OGR.createMem []
     let size = 100
         burnValue = 10 :: Double
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs23030) "POINT (0 0)")
           geomBuffer 10 10 g
         feat = TestFeature geom (0 :: Double) (0 :: Double)
         env  = Envelope (-500) 500
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
            (northUpGeotransform size env)
     v `shouldSatisfy` U.any (==(Value burnValue))

   it "burns attribute from feature" $ do
     ds <- OGR.createMem []
     let size = 100
         Just geom = do
           g <- liftMaybe (geomFromWkt (Just srs23030) "POINT (0 0)")
           geomBuffer 10 10 g
         feat = TestFeature geom (15 :: Double) (0 :: Double)
         env  = Envelope (-500) 500
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
            srs23030
            size
            (northUpGeotransform size env)
     v `shouldSatisfy` U.any (==(Value (tfField1 feat)))

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
    , fdGeom   = GeomFieldDef WkbPolygon (Just srs23030) False
    , fdGeoms  = mempty}

