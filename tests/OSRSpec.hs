{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module OSRSpec (main, spec) where

import Data.Either (isRight, isLeft)
import qualified Data.Vector.Storable as St

import TestUtils

import OSR
import GDAL (XY(XY))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "SpatialReference" $ do

    it "can be created from EPSG number" $
      srsFromEPSG 23030 `shouldSatisfy` isRight

    it "can created from Proj4 string" $
      srsFromProj4 "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        `shouldSatisfy` isRight

    it "srsFromProj4 returns Left if invalid" $
      srsFromProj4 "foo" `shouldSatisfy` isLeft

  describe "CoordinateTransformation" $ do

    it "can be created" $ do
      let ct = do src <- srsFromEPSG 23030
                  dst <- srsFromEPSG 4326
                  coordinateTransformation src dst
      isRight ct `shouldBe` True

    it "can transform points" $ do
      let Right ct = do src <- srsFromEPSG 23030
                        dst <- srsFromEPSG 4326
                        coordinateTransformation src dst
          points :: St.Vector (XY Double)
          points   = [ XY 10000 10000
                     , XY 20000 20000]
          expected = [ XY (-7.399954586233987) 8.910802667504762e-2
                     , XY (-7.31036658723297)  0.17933194077993758]
      (points `transformWith` ct) `shouldBe` Just expected
