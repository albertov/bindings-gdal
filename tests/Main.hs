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
import Test.Framework.Providers.HUnit
import Test.HUnit

import Bindings.GDAL.Internal

main :: IO ()
main = setQuietErrorHandler >> registerAllDrivers >> $(defaultMainGenerator)


case_can_create_compressed_gtiff :: IO ()
case_can_create_compressed_gtiff
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- assertNotRaisesGDALException $ create "GTIFF" p 3000 3000 1 GDT_Int16
          [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    flushCache ds
    assertExistsAndSizeGreaterThan p 20000

case_can_create_and_open_dataset :: IO ()
case_can_create_and_open_dataset
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- assertNotRaisesGDALException $ create "GTIFF" p 100 100 1 GDT_Int16 []
    flushCache ds
    _ <- assertNotRaisesGDALException $ openReadOnly p
    return ()

case_can_create_and_createCopy_dataset :: IO ()
case_can_create_and_createCopy_dataset
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- assertNotRaisesGDALException $ createMem 100 100 1 GDT_Int16 []
    ds2 <- assertNotRaisesGDALException $ createCopy "GTIFF" p ds True []
    flushCache ds2
    assertExistsAndSizeGreaterThan p 0


case_can_get_existing_raster_band :: IO ()
case_can_get_existing_raster_band = do
    ds <- createMem 10 10 1 GDT_Int16 []
    assertNotRaisesGDALException $ withBand ds 1 (\_ -> return ())

case_can_set_and_get_geotransform :: IO ()
case_can_set_and_get_geotransform = do
    ds <- createMem 10 10 1 GDT_Int16 []
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    assertNotRaisesGDALException $ setDatasetGeotransform ds gt
    gt2 <- assertNotRaisesGDALException $ datasetGeotransform ds
    assertEqual "geotransform is not the same that was set" gt gt2

case_can_set_and_get_projection :: IO ()
case_can_set_and_get_projection = do
    ds <- createMem 10 10 1 GDT_Int16 []
    let proj = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
    assertNotRaisesGDALException $ setDatasetProjection ds proj
    proj2 <- datasetProjection ds
    assertEqual "projection is not the same that was set" proj proj2

case_can_set_and_get_nodata_value :: IO ()
case_can_set_and_get_nodata_value = do
    ds <- createMem 10 10 1 GDT_Int16 []
    withBand ds 1 $ \b -> do
        nodata <- bandNodataValue b
        assertBool "has unexpected nodata" (isNothing nodata)
        setBandNodataValue b (-1)
        nodata2 <- bandNodataValue b
        assertBool "nodata was not set" (isJust nodata2)
        assertEqual "nodata is not the same as set" (-1) (fromJust nodata2)

case_can_get_bandBlockSize :: IO ()
case_can_get_bandBlockSize = do
    ds <- createMem 10 10 1 GDT_Int16 []
    bsize <- withBand ds 1 (return . bandBlockSize)
    assertEqual "unexpected block size" (10, 1) bsize

case_can_get_bandSize :: IO ()
case_can_get_bandSize = do
    ds <- createMem 10 10 1 GDT_Int16 []
    bsize <- withBand ds 1 (return . bandSize)
    assertEqual "unexpected band size" (10, 10) bsize

case_cannot_get_nonexisting_raster_band :: IO ()
case_cannot_get_nonexisting_raster_band = do
    ds <- createMem 10 10 1 GDT_Int16 []
    assertRaisesGDALException $ withBand ds 2 undefined

case_can_get_rasterband_datatype :: IO ()
case_can_get_rasterband_datatype = do
    let dt = GDT_Int16
    ds <- createMem 10 10 1 dt []
    withBand ds 1 $ \band ->
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
    ds <- createMem 100 100 1 dtype []
    withBand ds 1 $ \band -> do
        assertNotRaisesGDALException $
            writeBand band 0 0 100 100 100 100 0 0 vec
        vec2 <- assertNotRaisesGDALException $
                   readBand band 0 0 100 100 100 100 0 0
        assertEqualVectors vec vec2

write_and_read_block dtype len vec = do
    ds <- createMem len 1 1 dtype []
    withBand ds 1 $ \band -> do
        assertNotRaisesGDALException $ writeBandBlock band 0 0 vec
        vec2 <- assertNotRaisesGDALException $ readBandBlock band 0 0
        assertBool "wrong type" (isJust vec2)
        assertEqualVectors vec (fromJust vec2)

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
        ds <- createMem 100 100 1 dt []
        withBand ds 1 $ \band -> do
            performGC -- try to segfault by causing premature calls to GDALClose
            assertNotRaisesGDALException $ fillBand band (fromIntegral value) 0
            v <- assertNotRaisesGDALException $
                    readBand band 0 0 100 100 100 100 0 0
            assertEqual "length mismatch" 10000 (St.length v)
            let allEqual = St.foldl' f True v
                f True a = a == value
                f False _ = False
            assertBool "read is different than filled" allEqual


case_readBandBlock_returns_Just_on_good_type :: IO ()
case_readBandBlock_returns_Just_on_good_type = do
    ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \band -> do
       good <- assertNotRaisesGDALException $
                  readBandBlock band 0 0 :: MaybeIOVector Int16
       assertBool "Could not read good type" (isJust good)

case_readBandBlock_returns_Nothing_on_bad_type :: IO ()
case_readBandBlock_returns_Nothing_on_bad_type = do
    ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \band -> do
       bad <- assertNotRaisesGDALException $
                readBandBlock band 0 0 :: MaybeIOVector Word8
       assertBool "Could read bad type" (isNothing bad)

case_writeBandBlock_fails_when_writing_bad_type :: IO ()
case_writeBandBlock_fails_when_writing_bad_type = do
    ds <- createMem 100 100 1 GDT_Int16 []
    withBand ds 1 $ \band -> do
       let v = St.replicate (bandBlockLen band) 0 :: St.Vector Word8
       assertRaisesGDALException $ writeBandBlock band 0 0 v

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

assertRaisesGDALException a = do
    r <- tryJust (guard . isGDALException) a
    case r of
      Left _  -> return ()
      Right _ -> assertBool "did not raise GDALException" False

assertNotRaisesGDALException a = do
    r <- tryJust (guard . isGDALException) a
    case r of
      Left e  -> assertBool ("raised exception: " ++ show e) False
                 >> return undefined
      Right v -> return v
