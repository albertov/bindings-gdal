{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDALSpec (main, spec) where

import Control.Monad (void)

import Data.Maybe (isNothing)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.String (fromString)

import System.FilePath (joinPath)

import GDAL
import OSR

import Test.QuickCheck (getPositive)
import Test.Hspec.QuickCheck (prop)
import TestUtils
import Arbitrary (InversibleGeotransform(..), (~==))


main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  itIO "runGDAL catches GDAL exceptions" $ do
    ret <- runGDAL (openReadOnly "foo.tif" GDT_Byte >> return False)
    ret `shouldSatisfy` either ((==OpenFailed) . gdalErrNum) id

  it "cannot open non-existent file" $
    openReadOnly "foo.tif" GDT_Byte
      `shouldThrow` ((==OpenFailed) . gdalErrNum)

  withDir "can create compressed gtiff" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    ds <- create "GTIFF" p 300 1 GDT_Int16 o
    flushCache ds
    p `existsAndSizeIsGreaterThan` 200

  withDir "can get filelist" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    create "GTIFF" p 30 1 GDT_Int16 []
      >>= datasetFileList >>= (`shouldBe` [fromString p])

  it "can get empty filelist" $
    createMem 30 1 GDT_Int16 [] >>= datasetFileList >>= (`shouldSatisfy` null)

  withDir "driver options are validated" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("zlevel", "bad level")]
        action = create "GTIFF" p 30 1 GDT_Int16 o
    action `shouldThrow` (==InvalidDriverOptions)

  withDir "can create and open dataset" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create "GTIFF" p 30 1 GDT_Int16 []
    flushCache ds
    void $ (openReadOnly p GDT_Int16)

  withDir "can create and copy dataset" $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- createMem 100 1 GDT_Int16 []
    createCopy "GTIFF" p ds True [] Nothing >>= flushCache
    p `existsAndSizeIsGreaterThan` 0

  describe "progress function" $ do
    withDir "can stop copy" $ \tmpDir -> do
      ds <- createMem 100 1 GDT_Int16 []
      let p = joinPath [tmpDir, "test.tif"]
          f = Just (\_ _ -> return Stop)
      createCopy "GTIFF" p ds True [] f `shouldThrow` isInterruptedException

    withDir "can throw exceptions" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem 100 1 GDT_Int16 []
      let crashIt = Just (error msg)
          msg     = "I crashed!"
      createCopy "GTIFF" p ds True [] crashIt
        `shouldThrow` (\e -> isBindingException e && isProgressFunException e)

    withDir "can report progress" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem 100 1 GDT_Int16 []
      msgsRef <- liftIO (newIORef [])
      let report pr m = do
            modifyIORef' msgsRef ((pr,m):)
            return Continue
      createCopy "GTIFF" p ds True [] (Just report) >>= flushCache
      p `existsAndSizeIsGreaterThan` 0
      liftIO (readIORef msgsRef) >>= (`shouldSatisfy` (not . null))

  it "can get band count" $
    createMem 10 5 GDT_Int16 [] >>= datasetBandCount >>= (`shouldBe` 5)

  it "can get existing raster band" $
    void $ createMem 10 1 GDT_Int16 [] >>= getBand 1

  it "cannot get non-existing raster band" $ do
    ds <- createMem 10 1 GDT_Int16 []
    getBand 2 ds `shouldThrow` ((== IllegalArg) . gdalErrNum)

  it "can add raster band" $ do
    ds <- createMem 10 1 GDT_Int16 []
    datasetBandCount ds >>= (`shouldBe` 1)
    void $ addBand [] ds
    datasetBandCount ds >>= (`shouldBe` 2)

  describe "datasetGeotransform" $ do

    it "can set and get" $ do
      ds <- createMem 10 1 GDT_Int16 []
      let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
      setDatasetGeotransform gt ds
      datasetGeotransform ds >>= (`shouldBe` Just gt)

    it "if not set get returns Nothing" $
      createMem 10 1 GDT_Int16 []
        >>= datasetGeotransform >>= (`shouldSatisfy` isNothing)

  describe "datasetProjection" $ do

    it "can set and get" $ do
      ds <- createMem 10 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
      setDatasetProjection proj ds
      datasetProjection ds >>= (`shouldBe` Just proj)

    it "returns Nothing if dataset has no projection" $ do
      ds <- createMem 10 1 GDT_Int16 []
      proj <- datasetProjection ds
      proj `shouldSatisfy` isNothing

  describe "datasetGCPs" $ do

    it "can set and get with srs" $ do
      ds <- createMem 10 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
          gcps = [gcp "1" (0 :+: 0) (45 :+: 21)]
      setDatasetGCPs gcps (Just proj) ds
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      Just proj `shouldBe` proj2

    it "can set and get with no srs" $ do
      ds <- createMem 10 1 GDT_Int16 []
      let gcps = [gcp "1" (0 :+: 0) (45 :+: 21)]
      setDatasetGCPs gcps Nothing ds
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      proj2 `shouldSatisfy` isNothing

    it "returns empty list and Nothing if dataset has no gcps" $ do
      ds <- createMem 10 1 GDT_Int16 []
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldSatisfy` null
      proj2 `shouldSatisfy` isNothing

  it "can set and get nodata value" $ do
    b <- getBand 1 =<< createMem 10 1 GDT_Int16 []
    bandNodataValue b >>= (`shouldSatisfy` isNothing)
    let nodataValue = (-1)
    setBandNodataValue nodataValue b
    bandNodataValue b >>= (`shouldBe` Just nodataValue)

  it "can get bandBlockSize" $ do
    b <- getBand 1 =<< createMem 10 1 GDT_Int16 []
    bandBlockSize b `shouldBe` (10 :+: 1)

  it "can get bandSize" $ do
    b <- getBand 1 =<< createMem 10 1 GDT_Int16 []
    bandSize b `shouldBe` 10


  describe "Geotransform" $ do

    prop "|$| is right associative" $ \(g1, g2, p) ->
      g1 |$| g2 |$| p ~== g1 |$| (g2 |$| p)

    prop "|$| with (inv gt) inverts |$| with gt" $
      \(InversibleGeotransform gt, p) -> inv gt |$| gt |$| p ~== p

    prop "can compose geotransforms" $ \(g1, g2, p) ->
      g1 |.| g2 |$| p ~== g1 |$| g2 |$| p

    describe "northUpGeotransform" $ do

      prop "pixel (0,0) is upper left corner" $ \(env, size) ->
        let gt = northUpGeotransform sz env
            sz = fmap getPositive size
            ul = pFst (envelopeMin env) :+: pSnd (envelopeMax env)
        in gt |$| 0 ~== ul

      prop "pixel (sizeX,0) upper right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            ur  = pFst (envelopeMax env) :+: pSnd (envelopeMax env)
        in gt |$| (pFst sz' :+: 0) ~== ur

      prop "pixel (0,sizeY) upper right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            ll  = pFst (envelopeMin env) :+: pSnd (envelopeMin env)
        in gt |$| (0  :+: pSnd sz') ~== ll

      prop "pixel (sizeX,sizeY) lower right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            lr  = pFst (envelopeMax env) :+: pSnd (envelopeMin env)
        in gt |$| (pFst sz' :+: pSnd sz') ~== lr

    describe "geoEnvelopeTransformer" $ do
      prop "produces valid envelope" $ \(InversibleGeotransform gt, env) ->
        case geoEnvelopeTransformer gt of
          Nothing -> False
          Just tr -> let Envelope (x0 :+: y0) (x1 :+: y1) = tr env
                     in x0 <= x1 && y0 <= y1

      prop "produces full size of northUpGeotransform" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
        in case geoEnvelopeTransformer gt of
            Nothing -> False
            Just tr -> case tr env of
                         Envelope (0 :+: 0) (x1 :+: y1) ->
                            x1 == pFst sz  && y1 == pSnd sz
                         _ -> False

  describe "metadata stuff" $ do

    describe "metadataDomains" $ do

      {- These tests are too version dependant and redundant
      it "mem driver dataset" $
        createMem 200 1 GDT_Int16 [] >>= metadataDomains >>= (`shouldBe` [])

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        doms <- metadataDomains ds
        if version > (1,11)
           then doms `shouldBe` ["IMAGE_STRUCTURE"]
           else doms `shouldBe` []
      -}

      withDir "GTIFF driver band" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        getBand 1 ds >>= metadataDomains >>= (`shouldBe` [])

    describe "metadata" $ do

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        meta <- metadata (Just "IMAGE_STRUCTURE") ds
        if version > (1,11)
           then meta `shouldBe` [("INTERLEAVE","BAND")]
           else meta `shouldBe` []

    describe "metadataItem" $ do

      withDir "GTIFF driver dataset (existing key)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        meta <- metadataItem (Just "IMAGE_STRUCTURE") "INTERLEAVE" ds
        if version > (1,11)
           then meta `shouldBe` (Just "BAND")
           else meta `shouldBe` Nothing

      withDir "GTIFF driver dataset (non-existing key)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        meta <- metadataItem (Just "IMAGE_STRUCTURE") "FOO" ds
        meta `shouldBe` Nothing

      withDir "GTIFF driver dataset (can set)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        setMetadataItem Nothing "foo" "bar" ds
        metadataItem Nothing "foo" ds >>= (`shouldBe` (Just "bar"))

    describe "driver metadata" $ do

      it "returns driver metadata" $ do
        driver <- driverByName "GTIFF"
        meta <- metadata Nothing driver
        meta `shouldSatisfy` (elem "DCAP_CREATE" . map fst)

    describe "description" $ do

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        let path = (joinPath [tmpDir, "foo"])
        create "GTIFF" path 200 1 GDT_Int16 []
          >>= description >>= (`shouldBe` (fromString path))

      withDir "GTIFF driver dataset (can set unicode)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        let someDesc = "ñamñamñamççççö"
        setDescription someDesc ds
        description ds >>= (`shouldBe` someDesc)

      withDir "GTIFF driver band" $ \tmpDir ->
        create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
          >>= getBand 1 >>= description >>= (`shouldBe` "")

      withDir "GTIFF driver band (can set)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 200 1 GDT_Int16 []
        b <- getBand 1 ds
        let someDesc = "hjgjhghjgjh,gjhgjhgl"
        setDescription someDesc b
        description b >>= (`shouldBe` someDesc)

