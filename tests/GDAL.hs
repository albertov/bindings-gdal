{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}

import Control.Monad (forM_, guard)
import Control.Exception (SomeException, tryJust, try)
import System.IO.Error (isDoesNotExistError)
import Data.Complex (Complex(..))
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

import OSGeo.GDAL.Internal

main :: IO ()
main = setQuietErrorHandler >> registerAllDrivers >> $(defaultMainGenerator)


case_can_create_compressed_gtiff :: IO ()
case_can_create_compressed_gtiff
  = withSystemTempDirectory "test." $ \tmpDir ->
    assertNotThrowsGDALException $ do
      let p = joinPath [tmpDir, "test.tif"]
          o = [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
      ds <- create GTIFF p 3000 3000 1 o :: IO (RWDataset Int16)
      flushCache ds
      assertExistsAndSizeGreaterThan p 20000

case_can_create_and_open_dataset :: IO ()
case_can_create_and_open_dataset
  = withSystemTempDirectory "test." $ \tmpDir ->
    assertNotThrowsGDALException $ do
      let p = joinPath [tmpDir, "test.tif"]
      ds <- create GTIFF p 100 100 1 [] :: IO (RWDataset Int16)
      flushCache ds
      _ <- openReadOnly p :: IO (RODataset Int16)
      return ()

case_can_create_and_createCopy_dataset :: IO ()
case_can_create_and_createCopy_dataset
  = withSystemTempDirectory "test." $ \tmpDir ->
    assertNotThrowsGDALException $ do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem 100 100 1 [] :: IO (RWDataset Int16)
      ds2 <- createCopy GTIFF p ds True []
      flushCache ds2
      assertExistsAndSizeGreaterThan p 0

case_can_get_band_count :: IO ()
case_can_get_band_count = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 5 [] :: IO (RWDataset Int16)
    assertEqual "unexpected number of bands" 5 (datasetBandCount ds)

case_can_get_existing_raster_band :: IO ()
case_can_get_existing_raster_band = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    withBand ds 1 (\_ -> return ())

case_dataset_is_created_with_correct_datatype :: IO ()
case_dataset_is_created_with_correct_datatype
  = assertNotThrowsGDALException $ do
    let assertType t b = assertEqual "unexpected datatype" t (bandDatatype b)
    ds <- createMem 10 10 1 [] :: IO (RWDataset Word8)
    withBand ds 1 (assertType GDT_Byte)
    ds1 <- createMem 10 10 1 [] :: IO (RWDataset Word16)
    withBand ds1 1 (assertType GDT_UInt16)
    ds2 <- createMem 10 10 1 [] :: IO (RWDataset Word32)
    withBand ds2 1 (assertType GDT_UInt32)
    ds3 <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    withBand ds3 1 (assertType GDT_Int16)
    ds4 <- createMem 10 10 1 [] :: IO (RWDataset Int32)
    withBand ds4 1 (assertType GDT_Int32)
    ds5 <- createMem 10 10 1 [] :: IO (RWDataset Float)
    withBand ds5 1 (assertType GDT_Float32)
    ds6 <- createMem 10 10 1 [] :: IO (RWDataset Double)
    withBand ds6 1 (assertType GDT_Float64)
    ds7 <- createMem 10 10 1 [] :: IO (RWDataset (Complex Int16))
    withBand ds7 1 (assertType GDT_CInt16)
    ds8 <- createMem 10 10 1 [] :: IO (RWDataset (Complex Int32))
    withBand ds8 1 (assertType GDT_CInt32)
    ds9 <- createMem 10 10 1 [] :: IO (RWDataset (Complex Float))
    withBand ds9 1 (assertType GDT_CFloat32)
    ds10 <- createMem 10 10 1 [] :: IO (RWDataset (Complex Double))
    withBand ds10 1 (assertType GDT_CFloat64)

case_can_set_and_get_geotransform :: IO ()
case_can_set_and_get_geotransform = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    setDatasetGeotransform ds gt
    gt2 <- datasetGeotransform ds
    assertEqual "geotransform is not the same that was set" gt gt2

case_can_set_and_get_projection :: IO ()
case_can_set_and_get_projection = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    let proj = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
    setDatasetProjection ds proj
    proj2 <- datasetProjection ds
    assertEqual "projection is not the same that was set" proj proj2

case_can_set_and_get_nodata_value :: IO ()
case_can_set_and_get_nodata_value = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    withBand ds 1 $ \b -> do
        nodata <- bandNodataValue b
        assertBool "has unexpected nodata" (isNothing nodata)
        setBandNodataValue b (-1)
        nodata2 <- bandNodataValue b
        assertBool "nodata was not set" (isJust nodata2)
        assertEqual "nodata is not the same as set" (-1) (fromJust nodata2)

case_can_get_bandBlockSize :: IO ()
case_can_get_bandBlockSize = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    bsize <- withBand ds 1 (return . bandBlockSize)
    assertEqual "unexpected block size" (10, 1) bsize

case_can_get_bandSize :: IO ()
case_can_get_bandSize = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    bsize <- withBand ds 1 (return . bandSize)
    assertEqual "unexpected band size" (10, 10) bsize

case_cannot_get_nonexisting_raster_band :: IO ()
case_cannot_get_nonexisting_raster_band = do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    assertThrowsGDALException $ withBand ds 2 undefined

case_can_get_rasterband_datatype :: IO ()
case_can_get_rasterband_datatype = assertNotThrowsGDALException $ do
    ds <- createMem 10 10 1 [] :: IO (RWDataset Int16)
    withBand ds 1 $ \band ->
        assertEqual "datatype mismatch" GDT_Int16 (bandDatatype band)

case_write_and_read_band_word8 :: IO ()
case_write_and_read_band_word8 = write_and_read_band vec
   where vec = St.generate 10000 fromIntegral :: St.Vector Word8

case_write_and_read_band_float :: IO ()
case_write_and_read_band_float = write_and_read_band vec
   where vec = St.generate 10000 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Float

case_write_and_read_band_double :: IO ()
case_write_and_read_band_double = write_and_read_band vec
   where vec = St.generate 10000 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Double

case_write_and_read_band_complex_float32 :: IO ()
case_write_and_read_band_complex_float32 = write_and_read_band vec
   where vec = St.generate 10000 fun
         vec :: St.Vector (Complex Float)
         fun i = (fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)

case_write_and_read_band_complex_int16 :: IO ()
case_write_and_read_band_complex_int16
  = write_and_read_band vec
    where vec = St.generate 10000 fun
          vec :: St.Vector (Complex Int16)
          fun i = (fromIntegral i) :+ (fromIntegral i)

write_and_read_band :: forall a . (Eq a , GDALType a)
  => St.Vector a -> IO ()
write_and_read_band vec = assertNotThrowsGDALException $ do
    ds <- createMem 100 100 1 [] :: IO (RWDataset a)
    withBand ds 1 $ \band -> do
        writeBand band 0 0 100 100 100 100 0 0 vec
        vec2 <- readBand band 0 0 100 100 100 100 0 0
        assertEqualVectors vec vec2

write_and_read_block :: forall a. (Eq a, GDALType a)
  => St.Vector a -> IO ()
write_and_read_block vec = assertNotThrowsGDALException $ do
    ds <- createMem (St.length vec) 1 1 []
    withBand ds 1 $ \band -> do
        writeBandBlock band 0 0 vec
        vec2 <- readBandBlock band 0 0
        assertEqualVectors vec vec2

case_write_and_read_block_double :: IO ()
case_write_and_read_block_double = write_and_read_block vec
   where vec = St.generate 100 $ \i -> 1.1 * fromIntegral i
         vec :: St.Vector Double

case_write_and_read_block_int16 :: IO ()
case_write_and_read_block_int16 = write_and_read_block vec
   where vec = St.generate 100 fromIntegral
         vec :: St.Vector Int16

case_write_and_read_block_cdouble :: IO ()
case_write_and_read_block_cdouble = write_and_read_block vec
   where vec = St.generate 100 $ \i -> fromIntegral i :+ fromIntegral (i+i)
         vec :: St.Vector (Complex Double)

case_write_and_read_block_cint16 :: IO ()
case_write_and_read_block_cint16 = write_and_read_block vec
   where vec = St.generate 100 $ \i -> fromIntegral i :+ fromIntegral (i+i)
         vec :: St.Vector (Complex Int16)

case_fill_and_read_band_int16 :: IO ()
case_fill_and_read_band_int16 = assertNotThrowsGDALException $ do
    forM_ ([-10..10] :: [Int16]) $ \value -> do
        ds <- createMem 100 100 1 [] :: IO (RWDataset Int16)
        withBand ds 1 $ \band -> do
            performGC -- try to segfault by causing premature calls to GDALClose
            fillBand band (fromIntegral value) 0
            v <- readBand band 0 0 100 100 100 100 0 0
            assertEqual "length mismatch" 10000 (St.length v)
            let allEqual = St.foldl' f True v
                f True a = a == value
                f False _ = False
            assertBool "read is different than filled" allEqual

case_open_throws_on_bad_type :: IO ()
case_open_throws_on_bad_type = do
    withSystemTempDirectory "test." $ \tmpDir -> do
      let p = joinPath [tmpDir, "test.tif"]
      ds <- create GTIFF p 100 100 1 [] :: IO (RWDataset Int16)
      flushCache ds
      assertThrowsGDALException (openReadOnly p :: IO (RODataset Word8))

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

assertThrowsGDALException :: forall a. IO a -> IO ()
assertThrowsGDALException a = do
    r <- tryJust (guard . isGDALException) a
    case r of
      Left _  -> return ()
      Right _ -> assertBool "did not throw GDALException" False

assertNotThrowsGDALException :: forall b. IO b -> IO b
assertNotThrowsGDALException a = do
    r <- try a
    case r of
      Left (e :: SomeException) -> do
        assertBool ("threw exception: " ++ show e) False
        return undefined
      Right v -> return v
