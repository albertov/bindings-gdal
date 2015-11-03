{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module GDALSpec (main, spec) where

import Control.Monad (void, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Maybe (isNothing)
import Data.Complex (Complex(..))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Int (Int16, Int32)
import Data.Proxy (Proxy(Proxy))
import Data.String (fromString)
import Data.Typeable (typeOf)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)

import GDAL
import OSR
import OGR (Envelope(..))

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

  it "cannot create GDT_Unknown dataset" $
    createMem (XY 100 100) 1 GDT_Unknown []
      `shouldThrow` (==UnknownRasterDataType)

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
    ds <- createMem (XY 100 100) 1 GDT_Int16 []
    ds2 <- createCopy "GTIFF" p ds True [] Nothing
    flushCache ds2
    p `existsAndSizeIsGreaterThan` 0

  describe "progress function" $ do
    withDir "can stop copy" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      let stopIt = Just (\_ _ -> return Stop)
      createCopy "GTIFF" p ds True [] stopIt `shouldThrow` (==CopyStopped)

    withDir "can throw exceptions" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      let crashIt = Just (error msg)
          msg     = "I crashed!"
      createCopy "GTIFF" p ds True [] crashIt `shouldThrow` errorCall msg

    withDir "can report progress" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
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
    ds <- createMem (XY 10 10) 5 GDT_Int16 []
    datasetBandCount ds >>= (`shouldBe` 5)

  it "can get existing raster band" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    void $ getBand 1 ds

  it "cannot get non-existing raster band" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    getBand 2 ds `shouldThrow` ((== IllegalArg) . gdalErrNum)

  it "can add raster band" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    datasetBandCount ds >>= (`shouldBe` 1)
    void (addBand ds GDT_Float64 [])
    datasetBandCount ds >>= (`shouldBe` 2)

  describe "datasetGeotransform" $ do

    it "can set and get" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
      setDatasetGeotransform ds gt
      gt2 <- datasetGeotransform ds
      Just gt `shouldBe` gt2

    it "if not set get returns Nothing" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      gt <- datasetGeotransform ds
      gt `shouldSatisfy` isNothing

  describe "datasetProjection" $ do

    it "can set and get" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
      setDatasetProjection ds proj
      proj2 <- datasetProjection ds
      Just proj `shouldBe` proj2

    it "returns Nothing if dataset has no projection" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      proj <- datasetProjection ds
      proj `shouldSatisfy` isNothing

  describe "datasetGCPs" $ do

    it "can set and get with srs" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      let Right proj = srsFromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
          gcps = [gcp "1" (XY 0 0) (XY 45 21)]
      setDatasetGCPs ds gcps (Just proj)
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      Just proj `shouldBe` proj2

    it "can set and get with no srs" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      let gcps = [gcp "1" (XY 0 0) (XY 45 21)]
      setDatasetGCPs ds gcps Nothing
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldBe` gcps
      proj2 `shouldSatisfy` isNothing

    it "returns empty list and Nothing if dataset has no gcps" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      (gcps2,proj2) <- datasetGCPs ds
      gcps2 `shouldSatisfy` null
      proj2 `shouldSatisfy` isNothing

  it "can set and get nodata value" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    (nd :: Maybe Int16) <- bandNodataValue b
    nd `shouldSatisfy` isNothing
    let nodataValue = (-1) :: Int16
    setBandNodataValue b nodataValue
    nodata2 <- bandNodataValue b
    nodata2 `shouldBe` Just nodataValue

  it "can get bandBlockSize" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    bandBlockSize b `shouldBe` (XY 10 1)

  it "can get bandSize" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    bandSize b `shouldBe` (XY 10 10)

  describe "band and block IO" $ do

    it "can write block and read band with automatic conversion" $ do
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      band <- getBand 1 ds
      let len = bandBlockLen band
          vec :: (U.Vector (Value Int16))
          vec = U.generate len (Value . fromIntegral)
          bs  = bandBlockSize band
      writeBandBlock band 0 vec
      vec2 <- readBand band (Envelope 0 bs) bs
      vec `shouldBe` U.map (fmap round) (vec2 :: U.Vector (Value Double))

    it "can write band and read band with automatic conversion" $ do
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      band <- getBand 1 ds
      let vec :: U.Vector (Value Double)
          vec = U.generate 10000 (Value . fromIntegral)
      writeBand band (allBand band) (bandSize band) vec
      vec2 <- readBand band (allBand band) (bandSize band)
      vec `shouldBe` vec2

    it "can fill and read band" $ do
      forM_ ([-10..10] :: [Int16]) $ \value -> do
        band <- getBand 1 =<< createMem (XY 100 100) 1 GDT_Int16 []
        fillBand value  band
        v <- readBand band (allBand band) (bandSize band)
        U.length v `shouldBe` 10000
        let allEqual = U.foldl' f True v
            f True (Value a) = a == value
            f _ _            = False
        allEqual `shouldBe` True

    withDir "throws GDALException when reading block with wrong type" $ \d -> do
      let p = joinPath [d, "test.tif"]
      ds <- create "GTIFF" p 100 1 GDT_Int16 []
      flushCache ds
      ds2 <- openReadOnly p
      band <- getBand 1 ds2
      let badAction =  do
            (_ :: U.Vector (Value Word8)) <- readBandBlock band 0
            return ()
      badAction `shouldThrow` isGDALException
      badAction `shouldThrow` (== (InvalidDataType GDT_Int16))

    withDir "throws GDALException when writing block with wrong type" $ \d -> do
      let p = joinPath [d, "test.tif"]
      ds <- create "GTIFF" p 100 1 GDT_Int32 []
      flushCache ds
      ds2 <- openReadWrite p
      band <- getBand 1 ds2
      let v :: U.Vector (Value Word8)
          v = U.replicate (bandBlockLen band) (Value 0)

      writeBandBlock band 0 v `shouldThrow` isGDALException
      writeBandBlock band 0 v
        `shouldThrow` (==(InvalidDataType GDT_Int32))

    let fWord8 = (Value . fromIntegral) :: Int -> Value Word8
    it_can_write_and_read_band  fWord8
    it_can_write_and_read_block fWord8
    it_can_foldl                fWord8 (+) 0 []
    it_can_foldl                fWord8 (+) 0 [("TILED", "YES")]

    let fWord16 = (Value . fromIntegral) :: Int -> Value Word16
    it_can_write_and_read_band  fWord16
    it_can_write_and_read_block fWord16
    it_can_foldl                fWord16 (+) 0 []
    it_can_foldl                fWord16 (+) 0 [("TILED", "YES")]

    let fWord32 = (Value . fromIntegral) :: Int -> Value Word32
    it_can_write_and_read_band  fWord32
    it_can_write_and_read_block fWord32
    it_can_foldl                fWord32 (+) 0 []
    it_can_foldl                fWord32 (+) 0 [("TILED", "YES")]

    let fInt16 = (Value . fromIntegral) :: Int -> Value Int16
    it_can_write_and_read_band  fInt16
    it_can_write_and_read_block fInt16
    it_can_foldl                fInt16 (+) 0 []
    it_can_foldl                fInt16 (+) 0 [("TILED", "YES")]

    let fInt32 = (Value . fromIntegral) :: Int -> Value Int32
    it_can_write_and_read_band  fInt32
    it_can_write_and_read_block fInt32
    it_can_foldl                fInt32 (+) 0 []
    it_can_foldl                fInt32 (+) 0 [("TILED", "YES")]

    let fFloat = (Value . (*1.1) . fromIntegral) :: Int -> Value Float
    it_can_write_and_read_band  fFloat
    it_can_write_and_read_block fFloat
    it_can_foldl                fFloat (+) 0 []
    it_can_foldl                fFloat (+) 0 [("TILED", "YES")]

    let fDouble = (Value . (*1.1) . fromIntegral) :: Int -> Value Double
    it_can_write_and_read_band  fDouble
    it_can_write_and_read_block fDouble
    it_can_foldl                fDouble (+) 0 []
    it_can_foldl                fDouble (+) 0 [("TILED", "YES")]


#ifdef STORABLE_COMPLEX
    let fCInt16 i = Value ((fromIntegral i  :+ fromIntegral (i + i)))
        fCInt16 :: Int -> Value (Complex Int16)
        f2C :: Num a
            => Value (Complex a) -> Value (Complex a) -> Value (Complex a)
        f2C (Value (ra :+ ia)) (Value (rb :+ ib)) = Value ((ra+rb) :+ (ia+ib))
        zC :: Num a => Value (Complex a)
        zC = Value (0 :+ 0)
    it_can_write_and_read_block fCInt16
    it_can_write_and_read_band  fCInt16
    it_can_foldl                fCInt16 f2C zC []
    it_can_foldl                fCInt16 f2C zC [("TILED", "YES")]

    let fCInt32 i = Value ((fromIntegral i  :+ fromIntegral (i + i)))
        fCInt32 :: Int -> Value (Complex Int32)
    it_can_write_and_read_block fCInt32
    it_can_write_and_read_band  fCInt32
    it_can_foldl                fCInt32 f2C zC []
    it_can_foldl                fCInt32 f2C zC [("TILED", "YES")]

    let fCFloat i = Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2))
        fCFloat :: Int -> Value (Complex Float)
    it_can_write_and_read_block fCFloat
    it_can_write_and_read_band  fCFloat
    it_can_foldl                fCFloat f2C zC []
    it_can_foldl                fCFloat f2C zC [("TILED", "YES")]

    let fCDouble i = Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2))
        fCDouble :: Int -> Value (Complex Double)
    it_can_write_and_read_block fCDouble
    it_can_write_and_read_band  fCDouble
    it_can_foldl                fCDouble f2C zC []
    it_can_foldl                fCDouble f2C zC [("TILED", "YES")]
#endif


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
            ul = XY (px (envelopeMin env)) (py (envelopeMax env))
        in gt |$| 0 ~== ul

      prop "pixel (sizeX,0) upper right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            ur  = XY (px (envelopeMax env)) (py (envelopeMax env))
        in gt |$| (XY (px sz') 0) ~== ur

      prop "pixel (0,sizeY) upper right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            ll  = XY (px (envelopeMin env)) (py (envelopeMin env))
        in gt |$| (XY 0 (py sz')) ~== ll

      prop "pixel (sizeX,sizeY) lower right corner" $ \(env, size) ->
        let gt  = northUpGeotransform sz env
            sz  = fmap getPositive size
            sz' = fmap fromIntegral sz
            lr  = XY (px (envelopeMax env)) (py (envelopeMin env))
        in gt |$| (XY (px sz') (py sz')) ~== lr


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

      withDir "GTIFF driver dataset (can set)" $ \tmpDir -> do
        ds <- create "GTIFF" (joinPath [tmpDir, "foo"]) 3000 1 GDT_Int16 []
        let someDesc = "hjgjhghjgjh,gjhl"
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

it_can_write_and_read_band
  :: forall a. (Eq a , GDALType a)
  => (Int -> Value a) -> SpecWith (Arg (IO ()))
it_can_write_and_read_band f = it ("can write and read band " ++ typeName) $ do
  ds <- createMem (XY 100 100) 1 (dataType (Proxy :: Proxy a)) []
  band <- getBand 1 ds
  writeBand band (allBand band) (bandSize band) vec
  vec2 <- readBand band (allBand band) (bandSize band)
  vec `shouldBe` vec2
  where
    vec      = U.generate 10000 f
    typeName = show (typeOf (undefined :: a))



it_can_write_and_read_block
  :: forall a. (Eq a , GDALType a)
  => (Int -> Value a) -> SpecWith (Arg (IO ()))
it_can_write_and_read_block f = it ("can write and read block "++typeName) $ do
  ds <- createMem (XY (U.length vec) 1) 1 (dataType (Proxy :: Proxy a)) []
  band <- getBand 1 ds
  writeBandBlock band 0 vec
  vec2 <- readBandBlock band 0
  vec `shouldBe` vec2
  where
    vec      = U.generate 10000 f
    typeName = show (typeOf (undefined :: a))

it_can_foldl
  :: forall a. (Eq a, GDALType a)
  => (Int -> Value a) -> (Value a -> Value a -> Value a) -> Value a
  -> OptionList -> SpecWith (Arg (IO ()))
it_can_foldl f f2 z options = withDir name $ \tmpDir -> do
  let p = joinPath [tmpDir, "test.tif"]
      sz = XY 200 205
  ds <- create "GTIFF" p sz 1 (dataType (Proxy :: Proxy a)) options
  let vec = U.generate (sizeLen sz) f
  band <- getBand 1 ds
  writeBand band (allBand band) sz vec
  flushCache ds
  value <- GDAL.foldl' f2 z band
  value `shouldBe` U.foldl' f2 z vec
  where name = "can foldl with options " ++ show options ++ " " ++ typeName
        typeName = show (typeOf (undefined :: a))

infix 4 ~==
(~==) :: (Fractional a, Ord a) => a -> a -> Bool
a ~== b = abs(a-b)<epsilon
  where epsilon = 1e-3
