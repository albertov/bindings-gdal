{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Control.Monad (forM_, guard)
import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import Data.Complex
import Data.Int
import Data.Word
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector.Storable as St

import System.Mem (performGC)
import System.IO
import System.IO.Temp
import System.FilePath

import Test.Framework.TH
-- import Test.QuickCheck
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit


import Bindings.GDAL.Internal

main :: IO ()
main = registerAllDrivers >> $(defaultMainGenerator)

case_can_create_compressed_gtiff :: IO ()
case_can_create_compressed_gtiff
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create "GTIFF" p 3000 3000 1 GDT_Int16
          [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    assertBool "Could not create dataset" (isJust ds)
    flushCache (fromJust ds)
    assertExistsAndSizeGreaterThan p 20000

case_can_create_and_open_dataset :: IO ()
case_can_create_and_open_dataset
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create "GTIFF" p 100 100 1 GDT_Int16 []
    assertBool "Could not create dataset" (isJust ds)
    flushCache (fromJust ds)
    ds2 <- open p GA_ReadOnly
    assertBool "Could not open dataset" (isJust ds2)

case_can_create_and_createCopy_dataset :: IO ()
case_can_create_and_createCopy_dataset
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- createMem 100 100 1 GDT_Int16 []
    assertBool "Could not create dataset" (isJust ds)
    ds2 <- createCopy "GTIFF" p (fromJust ds) True []
    assertBool "Could not copy dataset" (isJust ds2)
    flushCache (fromJust ds2)
    assertExistsAndSizeGreaterThan p 0


case_can_get_existing_raster_band :: IO ()
case_can_get_existing_raster_band = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    withBand ds 1 $ assertBool "Could not get band 1" . isJust

case_can_set_and_get_geotransform :: IO ()
case_can_set_and_get_geotransform = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    err <- setDatasetGeotransform ds gt
    assertEqual "setDatasetGeotransform returned error" CE_None err
    gt2 <- datasetGeotransform ds
    assertBool "error getting geotransform" (isJust gt2)
    assertEqual "geotransform is not the same that was set" gt (fromJust gt2)

case_can_set_and_get_projection :: IO ()
case_can_set_and_get_projection = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    let proj = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
    err <- setDatasetProjection ds proj
    assertEqual "setDatasetProjection returned error" CE_None err
    proj2 <- datasetProjection ds
    assertEqual "projection is not the same that was set" proj proj2

case_can_set_and_get_nodata_value :: IO ()
case_can_set_and_get_nodata_value = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    withBand ds 1 $ \(Just b) -> do
        nodata <- bandNodataValue b
        assertBool "has unexpected nodata" (isNothing nodata)
        setBandNodataValue b (-1)
        nodata2 <- bandNodataValue b
        assertBool "nodata was not set" (isJust nodata2)
        assertEqual "nodata is not the same as set" (-1) (fromJust nodata2)

case_can_get_bandBlockSize :: IO ()
case_can_get_bandBlockSize = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    bsize <- withBand ds 1 (return . bandBlockSize . fromJust)
    assertEqual "unexpected block size" (10, 1) bsize

case_can_get_bandSize :: IO ()
case_can_get_bandSize = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    bsize <- withBand ds 1 (return . bandSize . fromJust)
    assertEqual "unexpected band size" (10, 10) bsize

case_cannot_get_nonexisting_raster_band :: IO ()
case_cannot_get_nonexisting_raster_band = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    withBand ds 2 $ assertBool "Could get band 2" . not . isJust

case_can_get_rasterband_datatype :: IO ()
case_can_get_rasterband_datatype = do
    let dt = GDT_Int16
    Just ds <- createMem 10 10 1 dt []
    withBand ds 1 $ \(Just band) ->
        assertEqual "datatype mismatch" dt (bandDatatype band)

case_write_and_read_band_int16 :: IO ()
case_write_and_read_band_int16 = write_and_read_band GDT_Byte vec
   where vec = St.generate 10000 fromIntegral :: St.Vector Word8

case_write_and_read_band_float :: IO ()
case_write_and_read_band_float = write_and_read_band GDT_Float32 vec
   where vec = St.generate 10000 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Float

case_write_and_read_band_double :: IO ()
case_write_and_read_band_double = write_and_read_band GDT_Float64 vec
   where vec = St.generate 10000 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Double

case_write_and_read_band_complex_float32 :: IO ()
case_write_and_read_band_complex_float32 = write_and_read_band GDT_CFloat32 vec
   where vec = St.generate 10000 fun
         vec :: St.Vector (Complex Float)
         fun i = (fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)

case_sort_of_write_and_read_band_complex_int16 :: IO ()
case_sort_of_write_and_read_band_complex_int16
  = write_and_read_band GDT_CInt16 vec
    where vec = St.generate 10000 fun
          vec :: St.Vector (Complex Float)
          fun i = (fromIntegral i) :+ (fromIntegral i)


write_and_read_band dtype vec = do
    Just ds <- createMem 100 100 1 dtype []
    withBand ds 1 $ \(Just band) -> do
        err <- writeBand band 0 0 100 100 100 100 0 0 vec
        assertEqual "error writing band" err CE_None
        vec2' <- readBand band 0 0 100 100 100 100 0 0
        let vec2 = fromJust vec2'
        assertBool "error reading band" (isJust vec2')
        assertEqualVectors vec vec2

write_and_read_block dtype len vec = do
    Just ds <- createMem len 1 1 dtype []
    withBand ds 1 $ \(Just band) -> do
        err <- writeBandBlock band 0 0 vec
        assertEqual "error writing band" err CE_None
        vec2' <- readBandBlock band 0 0
        let vec2 = fromJust vec2'
        assertBool "error reading band" (isJust vec2')
        assertEqualVectors vec vec2

case_write_and_read_block_double :: IO ()
case_write_and_read_block_double = write_and_read_block GDT_Float64 100 vec
   where vec = St.generate 100 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Double

case_write_and_read_block_int16 :: IO ()
case_write_and_read_block_int16 = write_and_read_block GDT_Int16 100 vec
   where vec = St.generate 100 fromIntegral
         vec :: St.Vector Int16


case_fill_and_read_band_int16 :: IO ()
case_fill_and_read_band_int16 = do
    forM_ [GDT_Int16, GDT_Int32] $ \dt -> do
    forM_ ([-10..10] :: [Int16]) $ \value -> do
        Just ds <- createMem 100 100 1 dt []
        withBand ds 1 $ \(Just band) -> do
            performGC -- try to segfault by causing premature calls to GDALClose
            err <- fillBand band (fromIntegral value) 0
            assertEqual "error filling band" err CE_None
            v <- readBand band 0 0 100 100 100 100 0 0
            let v' = fromJust v
            assertBool "error reading band" (isJust v)
            assertEqual "length mismatch" 10000 (St.length v')
            let allEqual = St.foldl' f True v'
                f True a = a == value
                f False _ = False
            assertBool "read is different than filled" allEqual

case_fill_and_read_band_double :: IO ()
case_fill_and_read_band_double = do
    forM_ [GDT_Float32, GDT_Float64] $ \dt -> do
    forM_ ([(-10),(-9.5)..10] :: [Double]) $ \value -> do
        Just ds <- createMem 100 100 1 dt []
        withBand ds 1 $ \(Just band) -> do
            err <- fillBand band (realToFrac value) 0
            assertEqual "error filling band" err CE_None
            v <- readBand band 0 0 100 100 100 100 0 0
            let v' = fromJust v
            assertBool "error reading band" (isJust v)
            assertEqual "length mismatch" 10000 (St.length v')
            let allEqual = St.foldl' f True v'
                f True a = a == value
                f False _ = False
            assertBool "read is different than filled" allEqual


case_fill_and_read_band_byte :: IO ()
case_fill_and_read_band_byte = do
    forM_ [GDT_Byte, GDT_UInt16, GDT_UInt32] $ \dt -> do
    forM_ ([0..20] :: [Word8]) $ \value -> do
        Just ds <- createMem 100 100 1 dt []
        withBand ds 1 $ \(Just band) -> do
            performGC -- try to segfault by causing premature calls to GDALClose
            err <- fillBand band (fromIntegral value) 0
            assertEqual "error filling band" err CE_None
            v <- readBand band 0 0 100 100 100 100 0 0
            let v' = fromJust v
            assertBool "error reading band" (isJust v)
            assertEqual "length mismatch" 10000 (St.length v')
            let allEqual = St.foldl' f True v'
                f True a = a == value
                f False _ = False
            assertBool "read is different than filled" allEqual

case_readBandBlock_returns_Just_on_good_type :: IO ()
case_readBandBlock_returns_Just_on_good_type = do
    Just ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \(Just band) -> do
       good <- readBandBlock band 0 0 :: MaybeIOVector Int16
       assertBool "Could not read good type" (isJust good)

case_readBandBlock_returns_Nothing_on_bad_type :: IO ()
case_readBandBlock_returns_Nothing_on_bad_type = do
    Just ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \(Just band) -> do
       bad <- readBandBlock band 0 0 :: MaybeIOVector Word8
       assertBool "Could read bad type" (isNothing bad)

case_writeBandBlock_fails_when_writing_bad_type :: IO ()
case_writeBandBlock_fails_when_writing_bad_type = do
    Just ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \(Just band) -> do
       let v = St.replicate (bandblockLen band) 0 :: St.Vector Word8
       err <- writeBandBlock band 0 0 v
       assertBool "Could write bad block" $ CE_None /= err

--
-- Utils
--


assertEqualVectors :: (Eq a, St.Storable a)
  => St.Vector a
  -> St.Vector a
  -> IO ()
assertEqualVectors a b = assertBool "vectors are different" (sameL && areEqual)
  where areEqual  = St.foldl' f True $  St.zipWith (==) a b
        f True v  = v == True
        f False _ = False
        sameL     = St.length a == St.length b

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize p = do
    r <- tryJust (guard . isDoesNotExistError)
                 (withBinaryFile p ReadMode hFileSize)
    return (case r of Left _ -> Nothing; Right s -> Just s)

assertExistsAndSizeGreaterThan :: FilePath -> Integer -> IO ()
assertExistsAndSizeGreaterThan p s = do
    size <- getFileSize p
    assertBool ("file " ++ show p ++ " was not created") (isJust size)
    let s2 = fromJust size
    assertBool ("File size of " ++ show p ++ ", " ++ show s2 ++ " is <= " ++
                show s)
               (s2 > s)
