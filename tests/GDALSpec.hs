{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDALSpec (main, spec, setupAndTeardown) where

import Control.Applicative (liftA2, pure)
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Maybe (isNothing)
import Data.Complex (Complex(..))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Int (Int16, Int32)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Unboxed as U

import System.FilePath (joinPath)
import System.Mem (performMajorGC)

import Test.Hspec (
    Spec
  , SpecWith
  , Arg
  , after_
  , before_
  , describe
  , errorCall
  , hspec
  )

import GDAL
import GDAL.OSR

import TestUtils (
    shouldBe
  , shouldSatisfy
  , shouldThrow
  , existsAndSizeIsGreaterThan
  , it
  , withDir
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  it "cannot open non-existent file" $ do
    openReadOnly "foo.tif" `shouldThrow` ((==OpenFailed) . gdalErrNum)

  withDir "can create compressed gtiff" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    ds <- create GTIFF p (pure 3000) 1 o :: GDAL s (RWDataset s Int16)
    flushCache ds
    p `existsAndSizeIsGreaterThan` 20000
  
  withDir "can create and open dataset" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create GTIFF p (pure 100) 1 [] :: GDAL s (RWDataset s Int16)
    flushCache ds
    void $ (openReadOnly p :: GDAL s (RODataset s Int16))

  withDir "can create and copy dataset" $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
    ds2 <- createCopy GTIFF p ds True [] Nothing
    flushCache ds2
    p `existsAndSizeIsGreaterThan` 0

  describe "progress function" $ do
    withDir "can interrupt copy" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      let stopIt = Just (\_ _ -> return Stop)
      createCopy GTIFF p ds True [] stopIt `shouldThrow` (==CopyInterrupted)

    withDir "can throw exceptions" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      let crashIt = Just (error msg)
          msg     = "I crashed!"
      createCopy GTIFF p ds True [] crashIt `shouldThrow` errorCall msg

    withDir "can report progress" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      msgsRef <- liftIO (newIORef [])
      let report pr m = do
            modifyIORef' msgsRef ((pr,m):)
            return Continue
      ds2 <- createCopy GTIFF p ds True [] (Just report)
      flushCache ds2
      p `existsAndSizeIsGreaterThan` 0
      msgs <- liftIO (readIORef msgsRef)
      msgs `shouldSatisfy` (not . null)

  it "can get band count" $ do
    ds <- createMem (XY 10 10) 5 [] :: GDAL s (RWDataset s Int16)
    datasetBandCount ds `shouldBe` 5

  it "can get existing raster band" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    void $ getBand 1 ds

  it "cannot get non-existing raster band" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    getBand 2 ds `shouldThrow` ((== IllegalArg) . gdalErrNum)

  it "creates dataset with the correct datatype" $ do
    let shouldHaveType a t = a >>= (\b -> bandDatatype b `shouldBe` t)

    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Word8)
    getBand 1 ds `shouldHaveType` GDT_Byte

    ds1 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Word16)
    getBand 1 ds1 `shouldHaveType` GDT_UInt16

    ds2 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Word32)
    getBand 1 ds2 `shouldHaveType` GDT_UInt32

    ds3 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    getBand 1 ds3 `shouldHaveType` GDT_Int16

    ds4 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int32)
    getBand 1 ds4 `shouldHaveType` GDT_Int32

    ds5 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Float)
    getBand 1 ds5 `shouldHaveType` GDT_Float32

    ds6 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Double)
    getBand 1 ds6 `shouldHaveType` GDT_Float64

#ifdef STORABLE_COMPLEX
    ds7 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s (Complex Int16))
    getBand 1 ds7 `shouldHaveType` GDT_CInt16

    ds8 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s (Complex Int32))
    getBand 1 ds8 `shouldHaveType` GDT_CInt32

    ds9 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s (Complex Float))
    getBand 1 ds9 `shouldHaveType` GDT_CFloat32

    ds10 <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s (Complex Double))
    getBand 1 ds10 `shouldHaveType` GDT_CFloat64
#endif

  it "can set and get geotransform" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    setDatasetGeotransform ds gt
    gt2 <- datasetGeotransform ds
    gt `shouldBe` gt2

  describe "datasetProjection" $ do

    it "can set and get" $ do
      ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
      let Right proj = fromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
      setDatasetProjection ds proj
      proj2 <- datasetProjection ds
      Just proj `shouldBe` proj2

    it "returns Nothing if dataset has no projection" $ do
      ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
      proj <- datasetProjection ds
      proj `shouldSatisfy` isNothing

  it "can set and get nodata value" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    b <- getBand 1 ds
    nodata <- bandNodataValue b
    nodata `shouldSatisfy` isNothing
    let nodataValue = (-1)
    setBandNodataValue b nodataValue
    nodata2 <- bandNodataValue b
    nodata2 `shouldBe` Just nodataValue

  it "can get bandBlockSize" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    b <- getBand 1 ds
    bandBlockSize b `shouldBe` (XY 10 1)

  it "can get bandSize" $ do
    ds <- createMem (XY 10 10) 1 [] :: GDAL s (RWDataset s Int16)
    b <- getBand 1 ds
    bandSize b `shouldBe` (XY 10 10)

  it "can write block and read band with automatic conversion" $ do
    ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
    band <- getBand 1 ds
    let len = bandBlockLen band
        vec = U.generate len (Value . fromIntegral)
        bs  = bandBlockSize band
    writeBandBlock band (pure 0) vec
    vec2 <- readBand band (Window (pure 0) bs) bs
    vec `shouldBe` U.map (fmap round) (vec2 :: U.Vector (Value Double))

  it "can write band and read band with automatic conversion" $ do
    ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
    band <- getBand 1 ds
    let vec = U.generate 10000 (Value . fromIntegral) :: U.Vector (Value Double)
    writeBand band (allBand band) (bandSize band) vec
    vec2 <- readBand band (allBand band) (bandSize band)
    vec `shouldBe` vec2

  it "can fill and read band" $ do
    forM_ ([-10..10] :: [Int16]) $ \value -> do
      ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s Int16)
      band <- getBand 1 ds
      fillBand band (fromIntegral value) 0
      v <- readBand band (allBand band) (bandSize band)
      U.length v `shouldBe` 10000
      let allEqual = U.foldl' f True v
          f True (Value a) = a == value
          f _ _            = False
      allEqual `shouldBe` True

  withDir "throws GDALException when reading block with wrong type" $ \dir -> do
    let p = joinPath [dir, "test.tif"]
    ds <- create GTIFF p (pure 100) 1 [] :: GDAL s (RWDataset s Int16)
    flushCache ds
    ds2 <- openReadOnly p
    band <- getBand 1 ds2
    let badAction =  do
          (_ :: U.Vector (Value Word8)) <- readBandBlock band (pure 0)
          return ()
    badAction `shouldThrow` isGDALException
    badAction `shouldThrow` (== (InvalidDatatype GDT_Int16))

  withDir "throws GDALException when writing block with wrong type" $ \dir -> do
    let p = joinPath [dir, "test.tif"]
    ds <- create GTIFF p (pure 100) 1 [] :: GDAL s (RWDataset s Int32)
    flushCache ds
    ds2 <- openReadWrite p
    band <- getBand 1 ds2
    let v = U.replicate (bandBlockLen band) (Value 0) :: U.Vector (Value Word8)
    writeBandBlock band (pure 0) v `shouldThrow` isGDALException
    writeBandBlock band (pure 0) v `shouldThrow` (== (InvalidDatatype GDT_Int32))

  it_can_foldl [("TILED", "YES")]

  it_can_foldl []

  it_can_write_and_read_band  ((Value . fromIntegral) :: Int -> Value Word8)
  it_can_write_and_read_block ((Value . fromIntegral) :: Int -> Value Word8)

  it_can_write_and_read_band  ((Value . fromIntegral) :: Int -> Value Word16)
  it_can_write_and_read_block ((Value . fromIntegral) :: Int -> Value Word16)

  it_can_write_and_read_band  ((Value . fromIntegral) :: Int -> Value Word32)
  it_can_write_and_read_block ((Value . fromIntegral) :: Int -> Value Word32)

  it_can_write_and_read_band  ((Value . fromIntegral) :: Int -> Value Int16)
  it_can_write_and_read_block ((Value . fromIntegral) :: Int -> Value Int16)

  it_can_write_and_read_band  ((Value . fromIntegral) :: Int -> Value Int32)
  it_can_write_and_read_block ((Value . fromIntegral) :: Int -> Value Int32)

  it_can_write_and_read_band
    ((Value . (*1.1) . fromIntegral) :: Int -> Value Float)
  it_can_write_and_read_block
    ((Value . (*1.1) . fromIntegral) :: Int -> Value Float)

  it_can_write_and_read_band
    ((Value . (*1.1) . fromIntegral) :: Int -> Value Double)
  it_can_write_and_read_block
    ((Value . (*1.1) . fromIntegral) :: Int -> Value Double)

#ifdef STORABLE_COMPLEX
  it_can_write_and_read_band
    ((\i -> Value ((fromIntegral i  :+ fromIntegral (i + i))))
      :: Int -> Value (Complex Int16))
  it_can_write_and_read_block
    ((\i -> Value ((fromIntegral i  :+ fromIntegral (i + i))))
      :: Int -> Value (Complex Int16))

  it_can_write_and_read_band
    ((\i -> Value ((fromIntegral i  :+ fromIntegral (i + i))))
      :: Int -> Value (Complex Int32))
  it_can_write_and_read_block
    ((\i -> Value ((fromIntegral i  :+ fromIntegral (i + i))))
      :: Int -> Value (Complex Int32))

  it_can_write_and_read_band
    ((\i -> Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)))
      :: Int -> Value (Complex Float))
  it_can_write_and_read_block
    ((\i -> Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)))
      :: Int -> Value (Complex Float))

  it_can_write_and_read_band
    ((\i -> Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)))
      :: Int -> Value (Complex Double))
  it_can_write_and_read_block
    ((\i -> Value ((fromIntegral i * 1.1) :+ (fromIntegral i * 2.2)))
      :: Int -> Value (Complex Double))
#endif


-- | Makes sure (or tries) that we're not double-freeing, etc by destroying
--   the driver manager after every test and peformimg a major garbage
--   collection to force (really?) the finalizers to run.
setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown
  = before_ (setQuietErrorHandler >> registerAllDrivers)
  . after_  (destroyDriverManager >> performMajorGC)


it_can_write_and_read_band
  :: forall a. (Typeable a, Show a, Eq a , GDALType a)
  => (Int -> Value a) -> SpecWith (Arg (IO ()))
it_can_write_and_read_band f = it ("can write and read band " ++ typeName) $ do
  ds <- createMem (XY 100 100) 1 [] :: GDAL s (RWDataset s a)
  band <- getBand 1 ds
  writeBand band (allBand band) (bandSize band) vec
  vec2 <- readBand band (allBand band) (bandSize band)
  vec `shouldBe` vec2
  where
    vec      = U.generate 10000 f
    typeName = show (typeOf (undefined :: a))



it_can_write_and_read_block
  :: forall a. (Typeable a, Show a, Eq a , GDALType a)
  => (Int -> Value a) -> SpecWith (Arg (IO ()))
it_can_write_and_read_block f = it ("can write and read block "++typeName) $ do
  ds <- createMem (XY (U.length vec) 1) 1 []
  band <- getBand 1 ds
  writeBandBlock band (pure 0) vec
  vec2 <- readBandBlock band (pure 0)
  vec `shouldBe` vec2
  where
    vec      = U.generate 10000 f
    typeName = show (typeOf (undefined :: a))

it_can_foldl :: OptionList -> SpecWith (Arg (IO ()))
it_can_foldl options = withDir name $ \tmpDir -> do
  let p = joinPath [tmpDir, "test.tif"]
      sz = XY 300 305
  ds <- create GTIFF p sz 1 options
  let vec = U.generate (sizeLen sz) (Value . fromIntegral)
      vec :: U.Vector (Value Double)
  band <- getBand 1 ds
  writeBand band (allBand band) sz vec
  flushCache ds
  value <- GDAL.foldl' (liftA2 (+)) (Value 0) band
  let expected = U.foldl' (liftA2 (+)) (Value 0) vec
  value `shouldBe` expected
  where name = "can foldl with options " ++ show options
