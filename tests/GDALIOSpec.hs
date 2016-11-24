{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDALIOSpec (main, spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)

import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)

import GDAL
import OGR (envelopeSize)

import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do


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
ioSpec
  :: GDALType (HsType d)
  => DataType d -> (Int -> Value (HsType d)) -> SpecWith (Arg (IO ()))
ioSpec dt f = do
  it_can_write_and_read_block dt f
  it_can_write_and_read_band dt f
  it_can_foldl dt f
  it_can_foldlWindow dt f

it_can_write_and_read_band
  :: GDALType (HsType d)
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
  :: GDALType (HsType d)
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
  :: forall d. GDALType (HsType d)
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
  :: forall d. GDALType (HsType d)
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