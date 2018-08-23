{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GDAL.VRTSpec (main, spec) where

import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad (void)
import Data.Text (unpack)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as MV

import GDAL
import GDAL.VRT

import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $  do
  describe "function source" $ do

    it "can set value" $ do
      let sz = 10; val = 9876.54367543217678543212
      ds <- createVRT sz $ \vDs -> do
        b <- createVRTBand GDT_Float64 vDs
        addFuncSource b $ \env mVec -> do
          let (nx:+:ny) = envelopeSize env
          MV.length mVec `shouldBe` (nx*ny)
          MV.set mVec val
      b <- getBand 1 ds
      v <- readBand b (Envelope 0 sz) sz
      v `shouldSatisfy` V.all (==Value val)

      {-
       - Errs with: VRTFuncSource::RasterIO() - Irregular request.
      v2 <- readBand (b `bandAs` GDT_Int32) (Envelope 0 sz) sz
      v2 `shouldSatisfy` V.all (==Value (truncate val))
      -}

    it "can throw exceptions" $ do
      let sz = 10; msg = "oops"
          void' :: Monad m =>  m (U.Vector (Value Double)) -> m ()
          void' = void
      ds <- createVRT sz $ \vDs -> do
        b <- createVRTBand GDT_Float64 vDs
        addFuncSource b $ \_ _ -> throwIO (ErrorCall msg)
      b <- getBand 1 ds
      void' (readBand b (Envelope 0 sz) sz)
        `shouldThrow` ((==msg).unpack.gdalErrMsg)

  describe "simple source" $ do
    it "can combine two sources" $ do
      let sz1 = 10; sz2 = 15; sz3 = (+) <$> sz1 <*> sz2
      ds1 <-createMem sz1 1 GDT_Int32 []
      band1 <- getBand 1 ds1
      ds2 <-createMem sz2 1 GDT_Int32 []
      band2 <- getBand 1 ds2
      let vec1 = U.generate (product sz1)
                (Value . fromIntegral)
          vec2 = U.generate (product sz2)
                (Value . fromIntegral . (+5))
      writeBand band1 (allBand band1) (bandSize band1) vec1
      writeBand band2 (allBand band2) (bandSize band2) vec2
      ds3 <- createVRT sz3 $ \vDs -> do
        b <- createVRTBand GDT_Int32 vDs
        addSimpleSource b     (Envelope 0 sz1)
                        band1 (allBand band1)
                        ResNearest Nothing
        addSimpleSource b     (Envelope sz1 sz3)
                        band2 (allBand band2)
                        ResNearest Nothing
      band3 <- getBand 1 ds3
      let vProd a v = a * fromValue 1 v
          expected = V.foldl' vProd 1 (V.concat [vec1,vec2])
      s <- foldl' vProd 1 band3 
      s `shouldBe`  expected

    it "can combine two sources with mixed nodata" $ do
      let sz1 = 10; sz2 = 15; sz3 = (+) <$> sz1 <*> sz2
          nodata1 = -13; nodata2 = -7
      ds1 <- createMem sz1 1 GDT_Int32 []
      band1 <- getBand 1 ds1
      setBandNodataValue nodata1 band1
      ds2 <-createMem sz2 1 GDT_Int32 []
      band2 <- getBand 1 ds2
      setBandNodataValue nodata2 band2
      let vec1 = U.generate (product sz1)
                (ifEven fromIntegral)
          vec2 = U.generate (product sz2)
                (ifEven (fromIntegral . (+5)))
          ifEven f n | n `mod` 2 == 0 = Value (f n)
                     | otherwise      = NoData
      writeBand band1 (allBand band1) (bandSize band1) vec1
      writeBand band2 (allBand band2) (bandSize band2) vec2
      ds3 <- createVRT sz3 $ \vDs -> do
        b <- createVRTBand GDT_Int32 vDs
        addSimpleSource b     (Envelope 0 sz1)
                        band1 (allBand band1)
                        ResAverage (Just nodata1)
        addSimpleSource b     (Envelope sz1 sz3)
                        band2 (allBand band2)
                        ResAverage (Just nodata2)
      band3 <- getBand 1 ds3
      let vProd a v = a * fromValue 1 v
          expected = V.foldl' vProd 1 (V.concat [vec1,vec2])
      s <- foldl' vProd 1 band3 
      s `shouldBe`  expected
