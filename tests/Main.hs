{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad (forM_)
import Control.Exception (handle, SomeException)
import Data.Int
import Data.Word
import Data.Maybe (fromJust, isJust)
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
main = $(defaultMainGenerator)

case_can_create_compressed_gtiff :: IO ()
case_can_create_compressed_gtiff
  = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        d = driverByName "GTIFF"
    assertBool "Could not load Gtiff driver" (isJust d)
    ds <- create (fromJust d) p 3000 3000 1 GDT_Int16
          [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    assertBool "Could not create dataset" (isJust ds)
    flushCache (fromJust ds)
    assertExistsAndSizeGreaterThan p 20000


case_can_get_existing_raster_band :: IO ()
case_can_get_existing_raster_band = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    withRasterBand ds 1 $ assertBool "Could not get band 1" . isJust

case_cannot_get_nonexisting_raster_band :: IO ()
case_cannot_get_nonexisting_raster_band = do
    Just ds <- createMem 10 10 1 GDT_Int16 []
    withRasterBand ds 2 $ assertBool "Could get band 2" . not . isJust

case_can_get_rasterband_datatype :: IO ()
case_can_get_rasterband_datatype = do
    let dt = GDT_Int16
    Just ds <- createMem 10 10 1 dt []
    withRasterBand ds 1 $ \(Just band) ->
        assertEqual "datatype mismatch" dt (getRasterDatatype band)

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


write_and_read_band dtype vec = do
    Just ds <- createMem 100 100 1 dtype []
    withRasterBand ds 1 $ \(Just band) -> do
        err <- writeBand band 0 0 100 100 100 100 0 0 vec
        assertEqual "error writing band" err CE_None
        vec2' <- readBand band 0 0 100 100 100 100 0 0
        let vec2 = fromJust vec2'
        assertBool "error reading band" (isJust vec2')
        assertEqualVectors vec vec2

assertEqualVectors :: (Eq a, St.Storable a)
  => St.Vector a
  -> St.Vector a
  -> IO ()
assertEqualVectors a b = assertBool "vectors are different" (sameL && areEqual)
  where areEqual  = St.foldl' f True $  St.zipWith (==) a b
        f True v  = v == True
        f False _ = False
        sameL     = St.length a == St.length b

case_fill_and_read_band_int16 :: IO ()
case_fill_and_read_band_int16 = do
    forM_ [GDT_Int16, GDT_Int32] $ \dt -> do
    forM_ ([-10..10] :: [Int16]) $ \value -> do
        Just ds <- createMem 100 100 1 dt []
        withRasterBand ds 1 $ \(Just band) -> do
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
        withRasterBand ds 1 $ \(Just band) -> do
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
        withRasterBand ds 1 $ \(Just band) -> do
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
--
-- Utils
--

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize p
    = handle (\(_ :: SomeException) -> return Nothing) $
        withBinaryFile p ReadMode $ \h -> Just <$> hFileSize h

assertExistsAndSizeGreaterThan :: FilePath -> Integer -> IO ()
assertExistsAndSizeGreaterThan p s = do
    size <- getFileSize p
    assertBool ("file " ++ show p ++ " was not created") (isJust size)
    let s2 = fromJust size
    assertBool ("File size of " ++ show p ++ ", " ++ show s2 ++ " is <= " ++
                show s)
               (s2 > s)
