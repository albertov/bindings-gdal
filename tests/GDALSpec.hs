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
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (typeOf)
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
    ds <- create GTIFF p (pure 3000) 1 GDT_Int16 o
    flushCache ds
    p `existsAndSizeIsGreaterThan` 20000

  withDir "driver options are validated" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        o = [("zlevel", "bad level")]
        action = create GTIFF p (pure 3000) 1 GDT_Int16 o
    action `shouldThrow` (==InvalidDriverOptions)

  withDir "can create and open dataset" $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
    ds <- create GTIFF p (pure 100) 1 GDT_Int16 []
    flushCache ds
    void $ (openReadOnly p)

  withDir "can create and copy dataset" $ \tmpDir -> do
    let p  = joinPath [tmpDir, "test.tif"]
    ds <- createMem (XY 100 100) 1 GDT_Int16 []
    ds2 <- createCopy GTIFF p ds True [] Nothing
    flushCache ds2
    p `existsAndSizeIsGreaterThan` 0

  describe "progress function" $ do
    withDir "can stop copy" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      let stopIt = Just (\_ _ -> return Stop)
      createCopy GTIFF p ds True [] stopIt `shouldThrow` (==CopyStopped)

    withDir "can throw exceptions" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
      let crashIt = Just (error msg)
          msg     = "I crashed!"
      createCopy GTIFF p ds True [] crashIt `shouldThrow` errorCall msg

    withDir "can report progress" $ \tmpDir -> do
      let p  = joinPath [tmpDir, "test.tif"]
      ds <- createMem (XY 100 100) 1 GDT_Int16 []
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
    ds <- createMem (XY 10 10) 5 GDT_Int16 []
    datasetBandCount ds `shouldBe` 5

  it "can get existing raster band" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    void $ getBand 1 ds

  it "cannot get non-existing raster band" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    getBand 2 ds `shouldThrow` ((== IllegalArg) . gdalErrNum)

  it "can set and get geotransform" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    let gt = Geotransform 5.0 4.0 3.0 2.0 1.0 0.0
    setDatasetGeotransform ds gt
    gt2 <- datasetGeotransform ds
    gt `shouldBe` gt2

  describe "datasetProjection" $ do

    it "can set and get" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      let Right proj = fromProj4
                          "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
      setDatasetProjection ds proj
      proj2 <- datasetProjection ds
      Just proj `shouldBe` proj2

    it "returns Nothing if dataset has no projection" $ do
      ds <- createMem (XY 10 10) 1 GDT_Int16 []
      proj <- datasetProjection ds
      proj `shouldSatisfy` isNothing

  it "can set and get nodata value" $ do
    ds <- createMem (XY 10 10) 1 GDT_Int16 []
    b <- getBand 1 ds
    (nodata :: Maybe Int16) <- bandNodataValue b
    nodata `shouldSatisfy` isNothing
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
      writeBandBlock band (pure 0) vec
      vec2 <- readBand band (Window (pure 0) bs) bs
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
      ds <- create GTIFF p (pure 100) 1 GDT_Int16 []
      flushCache ds
      ds2 <- openReadOnly p
      band <- getBand 1 ds2
      let badAction =  do
            (_ :: U.Vector (Value Word8)) <- readBandBlock band (pure 0)
            return ()
      badAction `shouldThrow` isGDALException
      badAction `shouldThrow` (== (InvalidDatatype GDT_Int16))

    withDir "throws GDALException when writing block with wrong type" $ \d -> do
      let p = joinPath [d, "test.tif"]
      ds <- create GTIFF p (pure 100) 1 GDT_Int32 []
      flushCache ds
      ds2 <- openReadWrite p
      band <- getBand 1 ds2
      let v :: U.Vector (Value Word8)
          v = U.replicate (bandBlockLen band) (Value 0)

      writeBandBlock band (pure 0) v `shouldThrow` isGDALException
      writeBandBlock band (pure 0) v
        `shouldThrow` (==(InvalidDatatype GDT_Int32))

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


-- | Makes sure (or tries) that we're not double-freeing, etc by destroying
--   the driver manager after every test and peformimg a major garbage
--   collection to force (really?) the finalizers to run.
setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown
  = before_ (registerAllDrivers)
  . after_  (performMajorGC >> destroyDriverManager)


it_can_write_and_read_band
  :: forall a. (Eq a , GDALType a)
  => (Int -> Value a) -> SpecWith (Arg (IO ()))
it_can_write_and_read_band f = it ("can write and read band " ++ typeName) $ do
  ds <- createMem (XY 100 100) 1 (datatype (Proxy :: Proxy a)) []
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
  ds <- createMem (XY (U.length vec) 1) 1 (datatype (Proxy :: Proxy a)) []
  band <- getBand 1 ds
  writeBandBlock band (pure 0) vec
  vec2 <- readBandBlock band (pure 0)
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
  ds <- create GTIFF p sz 1 (datatype (Proxy :: Proxy a)) options
  let vec = U.generate (sizeLen sz) f
  band <- getBand 1 ds
  writeBand band (allBand band) sz vec
  flushCache ds
  value <- GDAL.foldl' f2 z band
  value `shouldBe` U.foldl' f2 z vec
  where name = "can foldl with options " ++ show options ++ " " ++ typeName
        typeName = show (typeOf (undefined :: a))
