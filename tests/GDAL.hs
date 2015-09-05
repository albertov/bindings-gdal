{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}

import Control.Monad (forM_, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, tryJust, try)
import System.IO.Error (isDoesNotExistError)
import Data.Complex (Complex(..))
import Data.Int
import Data.Word
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector.Storable as St

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
    assertNotThrowsGDALException $ runGDAL $ do
      let p = joinPath [tmpDir, "test.tif"]
          o = [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
      ds <- create GTIFF p 3000 3000 1 o :: GDAL s (RWDataset s Int16)
      flushCache ds
      assertExistsAndSizeGreaterThan p 20000

case_can_create_and_open_dataset :: IO ()
case_can_create_and_open_dataset
  = withSystemTempDirectory "test." $ \tmpDir ->
    assertNotThrowsGDALException $ runGDAL $ do
      let p = joinPath [tmpDir, "test.tif"]
      ds <- create GTIFF p 100 100 1 [] :: GDAL s (RWDataset s Int16)
      flushCache ds
      _ <- openReadOnly p :: GDAL s (RODataset s Int16)
      return ()

case_can_create_and_createCopy_dataset :: IO ()
case_can_create_and_createCopy_dataset
  = withSystemTempDirectory "test." $ \tmpDir ->
    assertNotThrowsGDALException $ runGDAL $ do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem 100 100 1 [] :: GDAL s (RWDataset s Int16)
      ds2 <- createCopy GTIFF p ds True []
      flushCache ds2
      assertExistsAndSizeGreaterThan p 0

case_can_get_band_count :: IO ()
case_can_get_band_count = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 5 [] :: GDAL s (RWDataset s Int16)
    c <- datasetBandCount ds
    liftIO $ assertEqual "unexpected number of bands" 5 c

case_can_get_existing_raster_band :: IO ()
case_can_get_existing_raster_band = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    _ <- getBand ds 1
    return ()

case_dataset_is_created_with_correct_datatype :: IO ()
case_dataset_is_created_with_correct_datatype
  = assertNotThrowsGDALException $ runGDAL $ do
    let assertType t b = do dt <- bandDatatype b
                            liftIO $ assertEqual "unexpected datatype" t dt
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Word8)
    getBand ds 1 >>= assertType GDT_Byte
    ds1 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Word16)
    getBand ds1 1 >>= (assertType GDT_UInt16)
    ds2 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Word32)
    getBand ds2 1 >>= (assertType GDT_UInt32)
    ds3 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    getBand ds3 1 >>= (assertType GDT_Int16)
    ds4 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int32)
    getBand ds4 1 >>= (assertType GDT_Int32)
    ds5 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Float)
    getBand ds5 1 >>= (assertType GDT_Float32)
    ds6 <- createMem 10 10 1 [] :: GDAL s (RWDataset s Double)
    getBand ds6 1 >>= (assertType GDT_Float64)
    ds7 <- createMem 10 10 1 [] :: GDAL s (RWDataset s (Complex Int16))
    getBand ds7 1 >>= (assertType GDT_CInt16)
    ds8 <- createMem 10 10 1 [] :: GDAL s (RWDataset s (Complex Int32))
    getBand ds8 1 >>= (assertType GDT_CInt32)
    ds9 <- createMem 10 10 1 [] :: GDAL s (RWDataset s (Complex Float))
    getBand ds9 1 >>= (assertType GDT_CFloat32)
    ds10 <- createMem 10 10 1 [] :: GDAL s (RWDataset s (Complex Double))
    getBand ds10 1 >>= (assertType GDT_CFloat64)

case_can_set_and_get_geotransform :: IO ()
case_can_set_and_get_geotransform = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    setDatasetGeotransform ds gt
    gt2 <- datasetGeotransform ds
    liftIO $ assertEqual "geotransform is not the same that was set" gt gt2

case_can_set_and_get_projection :: IO ()
case_can_set_and_get_projection = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    let proj = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
    setDatasetProjection ds proj
    proj2 <- datasetProjection ds
    liftIO $ assertEqual "projection is not the same that was set" proj proj2

case_can_set_and_get_nodata_value :: IO ()
case_can_set_and_get_nodata_value = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    getBand ds 1 >>= \b -> do
        nodata <- bandNodataValue b
        liftIO $ assertBool "has unexpected nodata" (isNothing nodata)
        setBandNodataValue b (-1)
        nodata2 <- bandNodataValue b
        liftIO $ assertBool "nodata was not set" (isJust nodata2)
        liftIO $ assertEqual "nodata is not the same as set" (-1) (fromJust nodata2)

case_can_get_bandBlockSize :: IO ()
case_can_get_bandBlockSize = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    b <- getBand ds 1
    sz <- bandBlockSize b
    liftIO $ assertEqual "unexpected block size" (10, 1) sz

case_can_get_bandSize :: IO ()
case_can_get_bandSize = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    b <- getBand ds 1
    sz <- bandSize b
    liftIO $ assertEqual "unexpected band size" (10, 10) sz

case_cannot_get_nonexisting_raster_band :: IO ()
case_cannot_get_nonexisting_raster_band = assertThrowsGDALException $
  runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    _ <- getBand ds 2
    return ()

case_can_get_rasterband_datatype :: IO ()
case_can_get_rasterband_datatype = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 10 10 1 [] :: GDAL s (RWDataset s Int16)
    getBand ds 1 >>= \band -> do
      dt <- bandDatatype band
      liftIO $ assertEqual "datatype mismatch" GDT_Int16 dt

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


case_can_write_one_type_and_read_another_with_automatic_conversion :: IO ()
case_can_write_one_type_and_read_another_with_automatic_conversion
  = assertNotThrowsGDALException $ runGDAL $ do
      ds <- createMem 100 100 1 [] :: GDAL s (RWDataset s Int16)
      getBand ds 1 >>= \band -> do
          len <- bandBlockLen band
          (x,y) <- bandBlockSize band
          let vec = St.generate len fromIntegral
          writeBandBlock band 0 0 vec
          vec2 <- readBand band 0 0 x y x y
          assertEqualVectors vec (St.map round (vec2 :: St.Vector Double))

case_can_write_and_read_with_automatic_conversion :: IO ()
case_can_write_and_read_with_automatic_conversion
  = assertNotThrowsGDALException $ runGDAL $ do
      ds <- createMem 100 100 1 [] :: GDAL s (RWDataset s Int16)
      let vec   = St.generate 10000 fromIntegral :: St.Vector Double
      getBand ds 1 >>= \band -> do
          writeBand band 0 0 100 100 100 100 vec
          vec2 <- readBand band 0 0 100 100 100 100
          assertEqualVectors vec vec2

write_and_read_band :: forall a . (Eq a , GDALType a)
  => St.Vector a -> IO ()
write_and_read_band vec = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem 100 100 1 [] :: GDAL s (RWDataset s a)
    getBand ds 1 >>= \band -> do
        writeBand band 0 0 100 100 100 100 vec
        vec2 <- readBand band 0 0 100 100 100 100
        assertEqualVectors vec vec2

write_and_read_block :: forall a. (Eq a, GDALType a)
  => St.Vector a -> IO ()
write_and_read_block vec = assertNotThrowsGDALException $ runGDAL $ do
    ds <- createMem (St.length vec) 1 1 []
    getBand ds 1 >>= \band -> do
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
case_fill_and_read_band_int16 = assertNotThrowsGDALException $ runGDAL $ do
    forM_ ([-10..10] :: [Int16]) $ \value -> do
        ds <- createMem 100 100 1 [] :: GDAL s (RWDataset s Int16)
        getBand ds 1 >>= \band -> do
            fillBand band (fromIntegral value) 0
            v <- readBand band 0 0 100 100 100 100
            liftIO $ assertEqual "length mismatch" 10000 (St.length v)
            let allEqual = St.foldl' f True v
                f True a = a == value
                f False _ = False
            liftIO $ assertBool "read is different than filled" allEqual

case_readBandBlock_throws_on_bad_type :: IO ()
case_readBandBlock_throws_on_bad_type = assertThrowsGDALException $
    withSystemTempDirectory "test." $ \tmpDir -> runGDAL $ do
      let p = joinPath [tmpDir, "test.tif"]
      ds <- create GTIFF p 100 100 1 [] :: GDAL s (RWDataset s Int16)
      flushCache ds
      ds2 <- openReadOnly p
      getBand ds2 1 >>= \band -> do
        (_ :: St.Vector Word8) <- readBandBlock band 0 0
        return ()

case_writeBandBlock_fails_when_writing_bad_type :: IO ()
case_writeBandBlock_fails_when_writing_bad_type = assertThrowsGDALException $
    withSystemTempDirectory "test." $ \tmpDir -> runGDAL $ do
      let p = joinPath [tmpDir, "test.tif"]
      ds <- create GTIFF p 100 100 1 [] :: GDAL s (RWDataset s Int16)
      flushCache ds
      ds2 <- openReadWrite p
      getBand ds2 1 >>= \band -> do
         len <- bandBlockLen band
         let v = St.replicate len 0 :: St.Vector Word8
         writeBandBlock band 0 0 v

--
-- Utils
--


assertEqualVectors :: (Eq a, St.Storable a)
  => St.Vector a
  -> St.Vector a
  -> GDAL s ()
assertEqualVectors a b
  = liftIO $ assertBool "vectors are different" (sameL && areEqual)
  where areEqual  = St.foldl' f True $  St.zipWith (==) a b
        f True v  = v == True
        f False _ = False
        sameL     = St.length a == St.length b

getFileSize :: FilePath -> GDAL s (Maybe Integer)
getFileSize p = liftIO $ do
    r <- tryJust (guard . isDoesNotExistError)
                 (withBinaryFile p ReadMode hFileSize)
    return (case r of Left _ -> Nothing; Right s -> Just s)

assertExistsAndSizeGreaterThan :: FilePath -> Integer -> GDAL s ()
assertExistsAndSizeGreaterThan p s = do
    size <- getFileSize p
    liftIO $ assertBool ("file " ++ show p ++ " was not created") (isJust size)
    let s2 = fromJust size
    liftIO $
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
