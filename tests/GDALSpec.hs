{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDALSpec (main, spec) where

import Control.Monad (void, liftM, forM_)

import Data.Maybe (isNothing)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.String (fromString)
import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)

import GDAL
import OSR

import Test.QuickCheck (getPositive)
import Test.Hspec.QuickCheck (prop)
import TestUtils
import Arbitrary (InversibleGeotransform(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  it "cannot open non-existent file" $ do
    openReadOnly "foo.tif" `shouldThrow` ((==OpenFailed) . gdalErrNum)

  withDir "can create compressed gtiff" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    ds <- create "GTIFF" p 3000 1 GDT_Int16 o
    flushCache ds
    p `existsAndSizeIsGreaterThan` 20000

  withDir "can get filelist" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create "GTIFF" p 3000 1 GDT_Int16 []
    fl <- datasetFileList ds
    fl `shouldBe` [fromString p]

  it "can get empty filelist" $ do
    ds <- createMem 3000 1 GDT_Int16 []
    fl <- datasetFileList ds
    fl `shouldSatisfy` null

  withDir "driver options are validated" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("zlevel", "bad level")]
        action = create "GTIFF" p 3000 1 GDT_Int16 o
    action `shouldThrow` (==InvalidDriverOptions)

  withDir "can create and open dataset" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create "GTIFF" p 3000 1 GDT_Int16 []
    flushCache ds
    void $ (openReadOnly p)

  withDir "can create and copy dataset" $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- createMem (100 :+: 100) 1 GDT_Int16 []
    ds2 <- createCopy "GTIFF" p ds True [] Nothing
    flushCache ds2
    p `existsAndSizeIsGreaterThan` 0

  describe "progress function" $ do
    withDir "can stop copy" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      let stopIt = Just (\_ _ -> return Stop)
      createCopy "GTIFF" p ds True [] stopIt
        `shouldThrow` isInterruptedException

    withDir "can throw exceptions" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      let crashIt = Just (error msg)
          msg     = "I crashed!"
      createCopy "GTIFF" p ds True [] crashIt
        `shouldThrow` (\e -> isBindingException e && isProgressFunException e)

    withDir "can report progress" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      msgsRef <- liftIO (newIORef [])
      let report pr m = do
            modifyIORef' msgsRef ((pr,m):)
            return Continue
      ds2 <- createCopy "GTIFF" p ds True [] (Just report)
      flushCache ds2
      p `existsAndSizeIsGreaterThan` 0
      msgs <- liftIO (readIORef msgsRef)
      msgs `shouldSatisfy` (not . null)

  it "can get band count" $ do
    ds <- createMem (10 :+: 10) 5 GDT_Int16 []
    datasetBandCount ds >>= (`shouldBe` 5)

  it "can get existing raster band" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    void $ getBand 1 ds

  it "cannot get non-existing raster band" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    getBand 2 ds `shouldThrow` ((== IllegalArg) . gdalErrNum)

  it "can add raster band" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    datasetBandCount ds >>= (`shouldBe` 1)
    void $ liftM (`bandTypedAs` GDT_Float64) (addBand ds [])
    datasetBandCount ds >>= (`shouldBe` 2)

  describe "datasetGeotransform" $ do

    it "can set and get" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
      setDatasetGeotransform ds gt
      gt2 <- datasetGeotransform ds
      Just gt `shouldBe` gt2

    it "if not set get returns Nothing" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      gt <- datasetGeotransform ds
      gt `shouldSatisfy` isNothing

  describe "datasetProjection" $ do

    it "can set and get" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
      setDatasetProjection ds proj
      proj2 <- datasetProjection ds
      Just proj `shouldBe` proj2

    it "returns Nothing if dataset has no projection" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      proj <- datasetProjection ds
      proj `shouldSatisfy` isNothing

  describe "datasetGCPs" $ do

    it "can set and get with srs" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
          gcps = [gcp "1" (0 :+: 0) (45 :+: 21)]
      setDatasetGCPs ds gcps (Just proj)
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      Just proj `shouldBe` proj2

    it "can set and get with no srs" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      let gcps = [gcp "1" (0 :+: 0) (45 :+: 21)]
      setDatasetGCPs ds gcps Nothing
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      proj2 `shouldSatisfy` isNothing

    it "returns empty list and Nothing if dataset has no gcps" $ do
      ds <- createMem (10 :+: 10) 1 GDT_Int16 []
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldSatisfy` null
      proj2 `shouldSatisfy` isNothing

  it "can set and get nodata value" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    nd <- bandNodataValue (b `bandTypedAs` GDT_Int16)
    nd `shouldSatisfy` isNothing
    let nodataValue = (-1)
    setBandNodataValue b nodataValue
    nodata2 <- bandNodataValue b
    nodata2 `shouldBe` Just nodataValue

  it "can get bandBlockSize" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    bandBlockSize b `shouldBe` (10 :+: 1)

  it "can get bandSize" $ do
    ds <- createMem (10 :+: 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    bandSize b `shouldBe` (10 :+: 10)

  describe "band and block IO" $ do

    it "readBandBlock converts type" $ do
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      band <- getBand 1 ds
      let len = bandBlockLen band
          vec = U.generate len (Value . fromIntegral)
          bs  = bandBlockSize band
      writeBandBlock (band `bandTypedAs` GDT_Int16) 0 vec
      vec2 <- readBand (band `bandCoercedTo` GDT_Float64) (Envelope 0 bs) bs
      vec `shouldBe` U.map (fmap round) vec2

    it "writeBandBlock converts type" $ do
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      band <- getBand 1 ds
      let len = bandBlockLen band
          vec = U.generate len (Value . fromIntegral)
          bs  = bandBlockSize band
      writeBandBlock (band `bandCoercedTo` GDT_Float64) 0 vec
      vec2 <- readBand (band `bandTypedAs` GDT_Int16) (Envelope 0 bs) bs
      vec2 `shouldBe` U.map (fmap round) vec

    it "can write and read band with automatic conversion" $ do
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      b <- getBand 1 ds
      let vec = U.generate 10000 (Value . fromIntegral)
      writeBand (b `bandTypedAs` GDT_Float64) (allBand b) (bandSize b) vec
      vec2 <- readBand b (allBand b) (bandSize b)
      vec `shouldBe` vec2

    describe "fillBand" $ do

      it "can fill and read band" $ do
        forM_ ([-10..10]) $ \value -> do
          band <- getBand 1 =<< createMem (100 :+: 100) 1 GDT_Int16 []
          fillBand (Value value)  (band `bandTypedAs` GDT_Int16)
          v <- readBand band (allBand band) (bandSize band)
          U.length v `shouldBe` 10000
          let allEqual = U.foldl' f True v
              f True (Value a) = a == value
              f _ _            = False
          allEqual `shouldBe` True

      it "can fill with NoData if setBandNodataValue" $ do
        band <- getBand 1 =<< createMem (100 :+: 100) 1 GDT_Int16 []
        setBandNodataValue (band `bandTypedAs` GDT_Int16) (-999)
        fillBand NoData band
        v <- readBand band (allBand band) (bandSize band)
        v `shouldSatisfy` (U.all isNoData)

      withDir "can fill with NoData if createBandMask" $ \d -> do
        ds <- create "GTIFF" (joinPath [d, "test.tif"]) 100 1 GDT_Int16 []
        band <- getBand 1 ds
        createBandMask band MaskPerDataset
        fillBand NoData (band `bandTypedAs` GDT_Int16)
        v <- readBand band (allBand band) (bandSize band)
        v `shouldSatisfy` (U.all isNoData)

      it "cannot fill with NoData if no nodata value or mask has been set" $ do
        band <- getBand 1 =<< createMem (100 :+: 100) 1 GDT_Int16 []
        fillBand NoData (band `bandTypedAs` GDT_Int16)
          `shouldThrow` (==BandDoesNotAllowNoData)

    it "can write and read block with automatic conversion" $ do
      ds <- createMem (100 :+: 100) 1 GDT_Int16 []
      band <- getBand 1 ds
      let vec = U.generate (bandBlockLen band) (Value . fromIntegral)
      writeBandBlock (band `bandTypedAs` GDT_Float64) 0 vec
      vec2 <- readBandBlock band 0
      vec `shouldBe` vec2

    describe "RasterBand IO" $ do
      let genIntegral :: Integral a => Int -> Value a
          genIntegral = Value . fromIntegral

          genReal :: RealFrac a => Int -> Value a
          genReal = Value . (*1.1) . fromIntegral

          genCIntegral :: Integral a => Int -> Value (Pair a)
          genCIntegral i = Value ((fromIntegral i  :+: fromIntegral (i + i)))

          genCReal :: RealFrac a => Int -> Value (Pair a)
          genCReal i =
            Value ((fromIntegral i * 1.1) :+: (fromIntegral i * 2.2))

      ioSpec GDT_Byte      genIntegral
      ioSpec GDT_UInt16    genIntegral
      ioSpec GDT_UInt32    genIntegral
      ioSpec GDT_Int16     genIntegral
      ioSpec GDT_Int32     genIntegral
      ioSpec GDT_Float32   genReal
      ioSpec GDT_Float64   genReal
      ioSpec GDT_CInt16    genCIntegral
      ioSpec GDT_CInt32    genCIntegral
      ioSpec GDT_CFloat32  genCReal
      ioSpec GDT_CFloat64  genCReal

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


  describe "metadata stuff" $ do

    describe "metadataDomains" $ do

      it "mem driver dataset" $ do
        ds <- createMem 3000 1 GDT_Int16 []
        doms <- metadataDomains ds
        doms `shouldBe` []

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        doms <- metadataDomains ds
        if version > (1,11)
           then doms `shouldBe` ["IMAGE_STRUCTURE"]
           else doms `shouldBe` []

      withDir "GTIFF driver band" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        b <- getBand 1 ds
        doms <- metadataDomains b
        doms `shouldBe` []

    describe "metadata" $ do

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        meta <- metadata (Just "IMAGE_STRUCTURE") ds
        if version > (1,11)
           then meta `shouldBe` [("INTERLEAVE","BAND")]
           else meta `shouldBe` []

    describe "metadataItem" $ do

      withDir "GTIFF driver dataset (existing key)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        meta <- metadataItem (Just "IMAGE_STRUCTURE") "INTERLEAVE" ds
        if version > (1,11)
           then meta `shouldBe` (Just "BAND")
           else meta `shouldBe` Nothing

      withDir "GTIFF driver dataset (non-existing key)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        meta <- metadataItem (Just "IMAGE_STRUCTURE") "FOO" ds
        meta `shouldBe` Nothing

      withDir "GTIFF driver dataset (can set)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        setMetadataItem Nothing "foo" "bar" ds
        meta <- metadataItem Nothing "foo" ds
        meta `shouldBe` (Just "bar")


    describe "description" $ do

      withDir "GTIFF driver dataset" $ \tmpDir -> do
        let path = (joinPath [tmpDir, "foo"])
        ds <- create "GTIFF" path 3000 1 GDT_Int16 []
        desc <- description ds
        desc `shouldBe` (fromString path)

      withDir "GTIFF driver dataset (can set unicode)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        let someDesc = "ñamñamñamççççö"
        setDescription someDesc ds
        desc <- description ds
        desc `shouldBe` someDesc

      withDir "GTIFF driver band" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        b <- getBand 1 ds
        desc <- description b
        desc `shouldBe` ""

      withDir "GTIFF driver band (can set)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        b <- getBand 1 ds
        let someDesc = "hjgjhghjgjh,gjhgjhgl"
        setDescription someDesc b
        desc <- description b
        desc `shouldBe` someDesc


ioSpec
  :: (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
ioSpec dt f = do
  it_can_write_and_read_block dt f
  it_can_write_and_read_band dt f
  it_can_foldl dt f

it_can_write_and_read_band
  :: (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
it_can_write_and_read_band dt f = do
  let typeName = show dt
      name = "can write and read band " ++ typeName
      sz = 300 :+: 307
      len = sizeLen sz

  describe name $ do

    it "all valid values" $ do
      ds <- createMem sz 1 dt []
      band <- getBand 1 ds
      let vec = U.generate len f
      writeBand band (allBand band) (bandSize band) vec
      flushCache ds
      vec2 <- readBand band (allBand band) (bandSize band)
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2

    it "with nodata value" $ do
      ds <- createMem sz 1 dt []
      band <- getBand 1 ds
      let vec = U.generate len (\i ->
                  if i > len`div`2 && f i /= nd
                     then f i
                     else NoData)
          nd@(Value noData) = f (-1)
      setBandNodataValue band noData
      writeBand band (allBand band) (bandSize band) vec
      flushCache ds
      vec2 <- readBand band (allBand band) (bandSize band)
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2

    withDir "with mask" $ \d -> do
      let path = joinPath [d, "test.tif"]
      ds <- create "GTIFF" path sz 1 dt []
      band <- getBand 1 ds
      let vec = U.generate len (\i -> if i < len`div`2 then f i else NoData)
      createBandMask band MaskPerBand
      writeBand band (allBand band) (bandSize band) vec
      flushCache ds
      vec2 <- readBand band (allBand band) (bandSize band)
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2


it_can_write_and_read_block
  :: (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
it_can_write_and_read_block dt f = forM_ optionsList $ \options -> do
  let typeName = show dt
      name = "can write and read block "++typeName++" (" ++ show options ++")"
      sz = 300 :+: 307

  describe name $ do

    withDir "all valid values" $ \d -> do
      let path = joinPath [d, "test.tif"]
      ds <- create "GTIFF" path sz 1 dt options
      band <- getBand 1 ds
      let vec = U.generate (bandBlockLen band) f
      writeBandBlock band 0 vec
      flushCache ds
      vec2 <- readBandBlock band 0
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2

    withDir "with nodata value" $ \d -> do
      let path = joinPath [d, "test.tif"]
      ds <- create "GTIFF" path sz 1 dt options
      band <- getBand 1 ds
      let vec = U.generate len (\i ->
                  if i > len`div`2 && f i /= nd
                     then f i
                     else NoData)
          nd@(Value noData) = f (-1)
          len = bandBlockLen band
      setBandNodataValue band noData
      writeBandBlock band 0 vec
      flushCache ds
      vec2 <- readBandBlock band 0
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2

    withDir "with mask" $ \d -> do
      let path = joinPath [d, "test.tif"]
      ds <- create "GTIFF" path sz 1 dt options
      band <- getBand 1 ds
      let vec = U.generate len (\i -> if i > len`div`2 then f i else NoData)
          len = bandBlockLen band
      createBandMask band MaskPerBand
      writeBandBlock band 0 vec
      flushCache ds
      vec2 <- readBandBlock band 0
      U.length vec `shouldBe` U.length vec2
      vec `shouldBe` vec2
  where optionsList = [[], [("TILED","YES")]]

it_can_foldl
  :: forall d. (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
it_can_foldl dt f = forM_ [[], [("TILED","YES")]] $ \options -> do

  let name = "can foldl with options " ++ show options ++ " " ++ typeName
      typeName = show dt

  describe name $ do

    withDir "all valid values" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      value <- GDAL.foldl' (+) 0 band
      value `shouldBe` U.foldl' (+) 0 vec

    withDir "with nodata value" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
          Value nodata = 0
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.imap (\i v -> if i>(sizeLen sz`div`2) then v else NoData)
                       (U.generate (sizeLen sz) f)
      band <- getBand 1 ds
      setBandNodataValue band nodata
      writeBand band (allBand band) sz vec
      flushCache ds
      value <- GDAL.foldl' (+) 0 band
      value `shouldBe` U.foldl' (+) 0 vec

    withDir "with mask" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.imap (\i v -> if i>(sizeLen sz`div`2) then v else NoData)
                       (U.generate (sizeLen sz) f)
      band <- getBand 1 ds
      createBandMask band MaskPerBand
      writeBand band (allBand band) sz vec
      flushCache ds
      value <- GDAL.foldl' (+) 0 band
      value `shouldBe` U.foldl' (+) 0 vec

infix 4 ~==
(~==) :: (Fractional a, Ord a) => a -> a -> Bool
a ~== b = abs(a-b)<epsilon
  where epsilon = 1e-3
