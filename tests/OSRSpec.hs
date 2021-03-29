{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module OSRSpec (spec) where

import Data.Either (isRight, isLeft)
import qualified Data.Vector.Storable as St

import TestUtils
import Arbitrary ((~==))

import OSR
import GDAL (Pair(..))

spec :: Spec
spec = do

  describe "SpatialReference" $ do

    itIO "can be created from EPSG number" $
      srsFromEPSG 23030 `shouldSatisfy` isRight

    itIO "can created from Proj4 string" $
      srsFromProj4 "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        `shouldSatisfy` isRight

    itIO "srsFromProj4 returns Left if invalid" $
      srsFromProj4 "foo" `shouldSatisfy` isLeft

  describe "CoordinateTransformation" $ do

    itIO "can be created" $ do
      let ct = do src <- srsFromEPSG 23030
                  dst <- srsFromEPSG 4326
                  coordinateTransformation src dst
      isRight ct `shouldBe` True

    itIO "can transform points" $ do
      Right dst <- srsFromEPSGIO 4326
      setAxisMappingStrategy dst OAMS_TRADITIONAL_GIS_ORDER
      let Right ct = do src <- srsFromEPSG 23030
                        coordinateTransformation src dst
          points :: St.Vector (Pair Double)
          points   = [ 10000 :+: 10000 , 20000 :+: 20000]
          expected = [ (-7.399954586233987) :+: 8.910802667504762e-2
                     , (-7.31036658723297)  :+: 0.17933194077993758]
          almostEq (a:+:b) (a':+:b') = a ~== a' && b ~== b'
      case points `transformWith` ct of
        Just result ->
          St.all id (St.zipWith almostEq result expected) `shouldBe` True
        Nothing     -> expectationFailure "transform returns Nothing"
