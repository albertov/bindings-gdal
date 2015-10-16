{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.OGRSpec (main, spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Either (isRight)

import System.Mem (performMajorGC)

import Test.Hspec (Spec, SpecWith, hspec, describe, before_, after_)

import GDAL (
    GDAL
  , setQuietErrorHandler
  , ErrorNum(..)
  , GDALException(..)
  )
import GDAL.OGR as OGR

import Paths_bindings_gdal

import TestUtils (
    shouldThrow
  , shouldBe
  , shouldNotBe
  , shouldContain
  , shouldSatisfy
  , it
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "Datasource and Layer" $ do

    it "cannot open a non-existent file" $ do
      openReadOnly "foo.shp" `shouldThrow` ((==OpenFailed) . gdalErrNum)

    it "can open a shape file" $ do
      void $ getShapePath >>= openReadOnly

    it "can get a layer by index" $ do
      void $ getShapePath >>= openReadOnly >>= getLayer 0

    it "cannot get a layer by wrong index" $ do
      (getShapePath >>= openReadOnly >>= getLayer (-1))
        `shouldThrow` (==InvalidLayerIndex (-1))

    it "can get a layer by name" $ do
      void $ getShapePath >>= openReadOnly >>= getLayerByName "fondo"

    it "cannot get a layer by wrong name" $ do
      (getShapePath >>= openReadOnly >>= getLayerByName "foo")
        `shouldThrow` (==InvalidLayerName "foo")

    it "can get layer count" $ do
      n <- getShapePath >>= openReadOnly >>= layerCount
      n `shouldBe` 1

    it "can get datasource name" $ do
      n <- getShapePath >>= openReadOnly >>= datasourceName
      n `shouldContain` "fondo"

  describe "executeSQL" $ do

    it "can execute a valid query with DefaultDialect" $ do
      ds <- getShapePath >>= openReadOnly
      void $ executeSQL DefaultDialect "SELECT * FROM fondo" Nothing ds

    it "throws error on invalid query" $ do
      ds <- getShapePath >>= openReadOnly
      (executeSQL DefaultDialect "dis is NoSQL!" Nothing ds)
        `shouldThrow` (\e -> case e of {SQLQueryError _ -> True; _ -> False})


  describe "Geometry" $ do

    describe "createFromWkt / exportToWkt" $ do

      it "succeeds if valid" $ do
        let eGeom = createFromWkt "POINT (34 21)" Nothing
        eGeom `shouldSatisfy` isRight

      it "fails if invalid" $ do
        let eGeom = createFromWkt "im not wkt" Nothing
        eGeom `shouldBe` Left UnsupportedGeometryType

      it "export is same as origin" $ do
        let Right g = createFromWkt wkt Nothing
            wkt     = "POINT (34 21)"
        exportToWkt g `shouldBe` wkt

    describe "createFromWkb / exportToWkb" $ do

      it "succeeds if valid" $ do
        let Right g = createFromWkt "POINT (34 21)" Nothing
            wkb     = exportToWkb WkbXDR g
        createFromWkb wkb Nothing `shouldSatisfy` isRight

      it "fails if invalid" $ do
        let eGeom = createFromWkb "im not wkb" Nothing
        eGeom `shouldBe` Left CorruptData


    it "compares equal when equal" $ do
      createFromWkt "POINT (2 5)" Nothing
        `shouldBe` createFromWkt "POINT (2 5)" Nothing

    it "compares not equal when not equal equal" $ do
      createFromWkt "POINT (2 6)" Nothing
        `shouldNotBe` createFromWkt "POINT (2 5)" Nothing

getShapePath :: GDAL s FilePath
getShapePath = liftIO $ getDataFileName "tests/fixtures/fondo.shp"

-- | Makes sure (or tries) that we're not double-freeing, etc by destroying
--   the driver manager after every test and peformimg a major garbage
--   collection to force (really?) the finalizers to run.
setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown
  = before_ (setQuietErrorHandler >> OGR.registerAllDrivers)
  . after_  (OGR.cleanupAll >> performMajorGC)
