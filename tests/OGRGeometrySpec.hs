{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module OGRGeometrySpec (spec) where

import Data.Either (isRight)
import Data.Maybe (isNothing)

-- We don't use TestUtils' to make sure it can be used outside of the GDAL
-- monad
import Test.Hspec

import OSR
import OGR

spec :: Spec
spec = parallel $ do

  describe "Geometry" $ parallel $ do

    describe "geomFromWkt / geomToWkt" $ parallel $ do

      it "succeeds if valid" $ do
        let eGeom = geomFromWkt Nothing "POINT (34 21)"
        eGeom `shouldSatisfy` isRight

      it "fails if invalid" $ do
        let eGeom = geomFromWkt Nothing "im not wkt"
        eGeom `shouldSatisfy` \case
          Left OGRException{ogrErrType=UnsupportedGeometryType} -> True
          _                                                     -> False

      it "export is same as original" $ do
        let Right g = geomFromWkt Nothing wkt
            wkt     = "POINT (34 21)"
        geomToWkt g `shouldBe` wkt

    describe "geomFromWkb / geomToWkb" $ parallel $ do

      it "succeeds if valid" $ do
        let Right g = geomFromWkt Nothing "POINT (34 21)"
            wkb     = geomToWkb WkbXDR g
        geomFromWkb Nothing wkb `shouldBe` Right g

      it "fails if invalid" $ do
        let eGeom = geomFromWkb Nothing "im not wkb"
        eGeom `shouldSatisfy` \case
          Left OGRException{ogrErrType=CorruptData} -> True
          _                                         -> False

    describe "geomFromGml / geomToGml" $ parallel $ do

      it "succeeds if valid" $ do
        let Right g = geomFromWkt Nothing "POINT (34 21)"
            gml     = geomToGml g
        geomFromGml gml `shouldBe` Right g

      it "fails if invalid" $ do
        let eGeom = geomFromGml "im not gml"
        eGeom `shouldSatisfy` \case
          Left OGRException{ogrErrType=CorruptData} -> True
          _                                         -> False


    it "compares equal when equal with no srs" $ do
      geomFromWkt Nothing "POINT (2 5)"
        `shouldBe` geomFromWkt Nothing "POINT (2 5)"

    it "compares equal when equal with srs" $ do
      let Right srs = srsFromWkt (srsToWkt srs23030)
      srs `shouldBe` srs23030
      geomFromWkt (Just srs) "POINT (2 5)"
        `shouldBe` geomFromWkt (Just srs23030) "POINT (2 5)"

    it "compares not equal when not equal" $ do
      geomFromWkt Nothing "POINT (2 6)"
        `shouldNotBe` geomFromWkt Nothing "POINT (2 5)"

    it "is projectable" $ do
      let Right g         = geomFromWkt Nothing "POINT (439466 4482586)"
          Right expected  = geomFromWkt (Just srs4326)
                              "POINT (-3.715491503365956 40.489899869998304)"
          Right coordTrans = coordinateTransformation srs23030 srs4326
      case g `transformWith` coordTrans of
        Nothing -> expectationFailure "Should have transformed the geom"
        Just t  -> do
          geomSpatialReference t `shouldBe` Just srs4326
          -- We compare WKT or else they won't match (TODO investigate why!)
          --t  `shouldBe` expected
          geomToWkt t  `shouldBe` geomToWkt expected

    describe "geomSpatialReference" $ parallel $ do

      it "is Nothing when it has no srs" $ do
        let Right g = geomFromWkt Nothing "POINT (34 21)"
        geomSpatialReference g `shouldSatisfy` isNothing

      it "is is the same as the one that was set" $ do
        let Right g = geomFromWkt (Just srs23030) "POINT (34 21)"
        geomSpatialReference g `shouldBe` Just srs23030

srs23030 :: SpatialReference
srs23030 = either exc id (srsFromEPSG 23030)
  where exc = error . ("Unexpected srsFromEPSG error: " ++) . show

srs4326 :: SpatialReference
srs4326 = either exc id (srsFromEPSG 4326)
  where exc = error . ("Unexpected srsFromEPSG error: " ++) . show
