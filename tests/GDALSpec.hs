{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDALSpec (main, spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, forM_)

import Data.Maybe (isNothing)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.String (fromString)
import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)

import GDAL
import OSR
import OGR (envelopeSize)

import Test.QuickCheck (getPositive)
import Test.Hspec.QuickCheck (prop)
import TestUtils
import Arbitrary (InversibleGeotransform(..), (~==))

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  it "cannot open non-existent file" $ do
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

  describe "band and block IO" $ do

    it "readBandBlock converts type" $ do
      band <- getBand 1 =<< createMem 100 1 GDT_Int16 []
      let len = bandBlockLen band
          vec = U.generate len (Value . fromIntegral)
          bs  = bandBlockSize band
      writeBandBlock band 0 vec
      vec2 <- readBand (band `bandAs` GDT_Float64) (Envelope 0 bs) bs
      vec `shouldBe` U.map (fmap round) vec2

    it "writeBandBlock converts type" $ do
      band <- createMem 100 1 GDT_Int16 [] >>= getBand 1
      let len = bandBlockLen band
          v   = U.generate len (Value . fromIntegral)
          bs  = bandBlockSize band
      writeBandBlock (band `bandAs` GDT_Float64) 0 v
      readBand band (Envelope 0 bs) bs >>= (`shouldBe` U.map (fmap round) v)

    it "can write and read band with automatic conversion" $ do
      b <- getBand 1 =<< createMem 100 1 GDT_Int16 []
      let vec = U.generate 10000 (Value . fromIntegral)
          b'  = b `bandAs` GDT_Float64
      writeBand b' (allBand b') (bandSize b') vec
      readBand b' (allBand b') (bandSize b') >>= (`shouldBe` vec)

    describe "fillBand" $ do

      it "can fill and read band" $ do
        forM_ ([-10..10]) $ \value -> do
          band <- getBand 1 =<< createMem 100 1 GDT_Int16 []
          fillBand (Value value)  band
          v <- readBand band (allBand band) (bandSize band)
          U.length v `shouldBe` 10000
          let allEqual = U.foldl' f True v
              f True (Value a) = a == value
              f _ _            = False
          allEqual `shouldBe` True

      it "can fill with NoData if setBandNodataValue" $ do
        band <- getBand 1 =<< createMem 100 1 GDT_Int16 []
        setBandNodataValue (-999) band
        fillBand NoData band
        readBand band (allBand band) (bandSize band)
          >>= (`shouldSatisfy` (U.all isNoData))

      withDir "can fill with NoData if createBandMask" $ \d -> do
        let path = joinPath [d, "test.tif"]
        band <- getBand 1 =<< create "GTIFF" path 100 1 GDT_Int16 []
        createBandMask MaskPerDataset band
        fillBand NoData band
        readBand band (allBand band) (bandSize band) >>=
          (`shouldSatisfy` (U.all isNoData))

      it "cannot fill with NoData if no nodata value or mask has been set" $
        (createMem 100 1 GDT_Int16 [] >>= getBand 1 >>= fillBand NoData)
          `shouldThrow` (==BandDoesNotAllowNoData)

    it "can write and read block with automatic conversion" $ do
      band <- getBand 1 =<< createMem 100 1 GDT_Int16 []
      let vec = U.generate (bandBlockLen band) (Value . fromIntegral)
      writeBandBlock band 0 vec
      readBandBlock band 0 >>= (`shouldBe` vec)

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

      describe "blockSource and blockSink" $ do

        it "can be used to copy a band" $ do
          ds <- createMem (333 :+: 444) 2 GDT_Int16 []
          band1 <- getBand 1 ds
          band2 <- getBand 2 ds
          let v1 = Value 0; v2 = Value 10
          fillBand v1 band1
          fillBand v2 band2
          runConduit (blockSource band2 =$= blockSink band1)
          readBand band1 (allBand band1) (bandSize band1)
            >>= (`shouldSatisfy` U.all (==v2))

        it "can zip two bands" $ do
          let sz = 333 :+: 444
              v1 = U.generate (sizeLen sz) (Value . fromIntegral . (+6))
              v2 = U.generate (sizeLen sz) (Value . fromIntegral . (*2))
          ds <- createMem sz 3 GDT_Int32 []
          b1 <- getBand 1 ds
          b2 <- getBand 2 ds
          b3 <- getBand 3 ds
          writeBand b1 (allBand b1) sz v1
          writeBand b2 (allBand b2) sz v2
          flushCache ds
          let conduit = getZipBlocks (fun <$> zipBlocks b1 <*> zipBlocks b2)
              fun = U.zipWith (+)
          runConduit (allBlocks b3 =$= conduit =$= blockSink b3)
          readBand b3 (allBand b3) sz >>= (`shouldBe` fun v1 v2)

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

ioSpec
  :: (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
ioSpec dt f = do
  it_can_write_and_read_block dt f
  it_can_write_and_read_band dt f
  it_can_foldl dt f
  it_can_foldlWindow dt f

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
      band <- getBand 1 =<< createMem sz 1 dt []
      let vec = U.generate len f
      writeBand band (allBand band) (bandSize band) vec
      readBand band (allBand band) (bandSize band) >>= (`shouldBe` vec)

    it "with nodata value" $ do
      band <- getBand 1 =<< createMem sz 1 dt []
      let vec = U.generate len (\i ->
                  if i > len`div`2 && f i /= nd
                     then f i
                     else NoData)
          nd@(Value noData) = f (-1)
      setBandNodataValue noData band
      writeBand band (allBand band) (bandSize band) vec
      readBand band (allBand band) (bandSize band) >>= (`shouldBe` vec)

    withDir "with mask" $ \d -> do
      let path = joinPath [d, "test.tif"]
      ds <- create "GTIFF" path sz 1 dt []
      band <- getBand 1 ds
      let vec = U.generate len (\i -> if i < len`div`2 then f i else NoData)
      createBandMask MaskPerBand band
      writeBand band (allBand band) (bandSize band) vec
      flushCache ds
      readBand band (allBand band) (bandSize band) >>= (`shouldBe` vec)


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
      setBandNodataValue noData band
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
      createBandMask MaskPerBand band
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
      GDAL.foldl' (+) 0 band >>= (`shouldBe` U.foldl' (+) 0 vec)

    withDir "with nodata value" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
          Value nodata = 0
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.imap (\i v -> if i>(sizeLen sz`div`2) then v else NoData)
                       (U.generate (sizeLen sz) f)
      band <- getBand 1 ds
      setBandNodataValue nodata band
      writeBand band (allBand band) sz vec
      flushCache ds
      GDAL.foldl' (+) 0 band >>= (`shouldBe` U.foldl' (+) 0 vec)

    withDir "with mask" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.imap (\i v -> if i>(sizeLen sz`div`2) then v else NoData)
                       (U.generate (sizeLen sz) f)
      band <- getBand 1 ds
      createBandMask MaskPerBand band
      writeBand band (allBand band) sz vec
      flushCache ds
      GDAL.foldl' (+) 0 band >>= (`shouldBe` U.foldl' (+) 0 vec)

it_can_foldlWindow
  :: forall d. (GDALType (HsType d), Num (HsType d))
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
it_can_foldlWindow dt f = forM_ [[], [("TILED","YES")]] $ \options -> do

  let name = "can foldlWindow with options " ++ show options ++ " " ++ typeName
      typeName = show dt

  describe name $ do

    withDir "is equivalent to foldl for whole band window" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      GDAL.foldlWindow' (+) 0 band (allBand band)
        >>= (`shouldBe` U.foldl' (+) 0 vec)

    withDir "works for proper subwindow" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      let env = Envelope 10 200
      val <- GDAL.foldlWindow' (+) 0 band env
      vec <- readBand band env (envelopeSize env)
      val `shouldBe` U.foldl' (+) 0 vec

    withDir "works for partially overlapping subwindow ll" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      let env = Envelope (-100) 200
          env2 = Envelope 0 200
      val <- GDAL.foldlWindow' (+) 0 band env
      vec <- readBand band env2 (envelopeSize env2)
      val `shouldBe` U.foldl' (+) 0 vec

    withDir "works for partially overlapping subwindow ur" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      let env = Envelope 150 300
          env2 = Envelope 150 sz
      val <- GDAL.foldlWindow' (+) 0 band env
      vec <- readBand band env2 (envelopeSize env2)
      val `shouldBe` U.foldl' (+) 0 vec

    withDir "works for non overlapping subwindow" $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
          sz = 200 :+: 205
      ds <- create "GTIFF" p sz 1 dt options
      let vec = U.generate (sizeLen sz) f
      band <- getBand 1 ds
      writeBand band (allBand band) sz vec
      flushCache ds
      let env = Envelope 1000 2000
      val <- GDAL.foldlWindow' (+) 0 band env
      val `shouldBe` 0
