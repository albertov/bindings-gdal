{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.OGRSpec (main, spec, setupAndTeardown) where

#include "bindings.h"

import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Catch (try, throwM)

import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Int
import Data.Word
import Data.Maybe (isNothing)
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable, typeOf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import System.Mem (performMajorGC)
import System.FilePath (joinPath)

import Test.Hspec (
    Spec
  , SpecWith
  , Arg
  , hspec
  , describe
  , before_
  , after_
  , afterAll_
  )

import GDAL (
    GDAL
  , ErrorNum(..)
  , GDALException(..)
  )
import GDAL.OGR as OGR
import GDAL.OSR (fromEPSG)

import Paths_bindings_gdal

import TestUtils (
    shouldThrow
  , shouldBe
  , shouldNotBe
  , shouldContain
  , shouldSatisfy
  , it
  , withDir
  , warn
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "DataSource and layer" $ do

    it "cannot open a non-existent file" $ do
      openReadOnly "foo.shp" `shouldThrow` ((==OpenFailed) . gdalErrNum)

    it "can open a shape file" $ do
      void $ getShapePath >>= openReadOnly

    it "can get datasource name" $ do
      n <- getShapePath >>= openReadOnly >>= datasourceName
      n `shouldContain` "fondo.shp"

    it "can get a layer by index" $ do
      void $ getShapePath >>= openReadOnly >>= getLayer 0

    it "cannot get a layer by wrong index" $ do
      (getShapePath >>= openReadOnly >>= getLayer (-1))
        `shouldThrow` (==InvalidLayerIndex (-1))

    it "can get a layer by name" $ do
      void $ getShapePath >>= openReadOnly >>= getLayerByName "fondo"

    it "cannot get a layer by wrong name" $ do
      (getShapePath >>= openReadOnly >>= getLayerByName "foo")
        `shouldThrow` (==InvalidLayerName "foo")

    it "can get layer count" $ do
      n <- getShapePath >>= openReadOnly >>= layerCount
      n `shouldBe` 1

    it "can get layer name" $ do
      n <- getShapePath >>= openReadOnly >>= getLayer 0 >>= layerName
      n `shouldBe` "fondo"

    withDir "can create ShapeFile" $ \d -> do
      let p = joinPath [d, "test.shp"]
      void $ create "ESRI Shapefile" p []

    it "create throws on invalid driver name" $
      create "foo" "" [] `shouldThrow` (==(UnknownDriver "foo"))

    describe "createLayerWithDef" $ do
      let check fd = do
            ds <- createMem []
            l <- createLayerWithDef ds fd StrictOK []
            layerFeatureDef l >>= (`shouldBe` fd)

      it "works with unicode name and field names" $
        check (FeatureDef { fdName   = "Barça Players"
                          , fdFields = [ strField "contraseña", realField "año"]
                          , fdGeom   = pointDef
                          , fdGeoms  = mempty})

      it "works with a single geometry field with no srs" $
        check (FeatureDef { fdName   = "Barça Players"
                          , fdFields = mempty
                          , fdGeom   = pointDef
                          , fdGeoms  = mempty})

      it "works with a single geometry field with srs" $ do
        let srs = either exc Just (fromEPSG 23030)
            exc = error . ("Unexpected fromEPSG error: " ++) . show
        check (FeatureDef { fdName   = "Barça Players"
                          , fdFields = mempty
                          , fdGeom   = pointDef {gfdSrs = srs}
                          , fdGeoms  = mempty})

      when canCreateMultipleGeometryFields $ do
        it "works with several geometry field with no srs" $ do
          check (FeatureDef { fdName   = "Barça Players"
                            , fdFields = mempty
                            , fdGeom   = pointDef
                            , fdGeoms  = [("another_geom", pointDef)]})

        it "works with several geometry field with srs" $ do
          let srs = either exc Just (fromEPSG 23030)
              exc = error . ("Unexpected fromEPSG error: " ++) . show
          check (FeatureDef { fdName   = "Barça Players"
                            , fdFields = mempty
                            , fdGeom   = pointDef
                            , fdGeoms  = [( "another_geom"
                                          , pointDef {gfdSrs=srs})]})
    describe "layer CRUD" $ do

      it "can create and retrieve a feature" $ do
        let fDef = FeatureDef { fdName   = "Some Ñçüo"
                              , fdFields = [strField "str" , intField "int"]
                              , fdGeom   = pointDef
                              , fdGeoms  = mempty}
            geom = either exc Just (createFromWkt Nothing "POINT (45 87)")
            exc  = error . ("Unexpected createFromWkt error: " ++) . show
            feat = Feature { fFields = [ ("str", OGRString "Avión")
                                       , ("int", OGRInteger 34)]
                           , fGeom   = geom
                           , fGeoms  = mempty}
        ds <- createMem []
        l <- createLayerWithDef ds fDef StrictOK []
        fid <- createFeature l feat
        getFeature l fid >>= (`shouldBe` Just feat)


  describe "getSpatialFilter" $ do

    it "return Noting when no filter has been set" $ do
      mGeom <- getShapePath >>= openReadOnly >>= getLayer 0 >>= getSpatialFilter
      mGeom `shouldSatisfy` isNothing

    it "can set a spatial filter and retrieve it" $ do
      l <- getShapePath >>= openReadWrite >>= getLayer 0
      let Right g = createFromWkt Nothing "POINT (34 21)"
      setSpatialFilter l g
      mGeom <- getSpatialFilter l
      mGeom `shouldBe` Just g


  describe "executeSQL" $ do

    it "can execute a valid query with DefaultDialect" $ do
      ds <- getShapePath >>= openReadOnly
      void $ executeSQL DefaultDialect "SELECT * FROM fondo" Nothing ds

    it "throws error on invalid query" $ do
      ds <- getShapePath >>= openReadOnly
      (executeSQL DefaultDialect "dis is NoSQL!" Nothing ds)
        `shouldThrow` (\e -> case e of {SQLQueryError _ -> True; _ -> False})


  describe "Geometry" $ do

    describe "createFromWkt / exportToWkt" $ do

      it "succeeds if valid" $ do
        let eGeom = createFromWkt Nothing "POINT (34 21)"
        eGeom `shouldSatisfy` isRight

      it "fails if invalid" $ do
        let eGeom = createFromWkt Nothing "im not wkt"
        eGeom `shouldBe` Left UnsupportedGeometryType

      it "export is same as origin" $ do
        let Right g = createFromWkt Nothing wkt
            wkt     = "POINT (34 21)"
        exportToWkt g `shouldBe` wkt

    describe "createFromWkb / exportToWkb" $ do

      it "succeeds if valid" $ do
        let Right g = createFromWkt Nothing "POINT (34 21)"
            wkb     = exportToWkb WkbXDR g
        createFromWkb Nothing wkb `shouldSatisfy` isRight

      it "fails if invalid" $ do
        let eGeom = createFromWkb Nothing "im not wkb"
        eGeom `shouldBe` Left CorruptData


    it "compares equal when equal" $ do
      createFromWkt Nothing "POINT (2 5)"
        `shouldBe` createFromWkt Nothing "POINT (2 5)"

    it "compares not equal when not equal" $ do
      createFromWkt Nothing "POINT (2 6)"
        `shouldNotBe` createFromWkt Nothing "POINT (2 5)"

  describe "OGRField instances" $
    forM_ (["Memory", "ESRI Shapefile"] :: [String]) $ \driverName -> do
#if SUPPORTS_WORD_FIELDS
      ogrFieldSpec driverName (34 :: Int)
      ogrFieldSpec driverName (Just 65 :: Maybe Int)
      ogrFieldSpec driverName (Nothing :: Maybe Int)
      ogrFieldSpec driverName (0 :: Int)
      ogrFieldSpec driverName (Just 0 :: Maybe Int)
      ogrFieldSpec driverName (minBound :: Int)
      ogrFieldSpec driverName (Just minBound :: Maybe Int)
      ogrFieldSpec driverName (maxBound :: Int)
      ogrFieldSpec driverName (Just maxBound :: Maybe Int)
      ogrFieldSpec driverName (34 :: Word)
      ogrFieldSpec driverName (Just 65 :: Maybe Word)
      ogrFieldSpec driverName (Nothing :: Maybe Word)
      ogrFieldSpec driverName (0 :: Int)
      ogrFieldSpec driverName (Just 0 :: Maybe Word)
      ogrFieldSpec driverName (minBound :: Word)
      ogrFieldSpec driverName (Just minBound :: Maybe Word)
      ogrFieldSpec driverName (maxBound :: Word)
      ogrFieldSpec driverName (Just maxBound :: Maybe Word)

      ogrFieldSpec driverName ([0,34,76,0,minBound,maxBound] :: [Int])
      ogrFieldSpec driverName (Just [0,65,maxBound,minBound] :: Maybe [Int])
      ogrFieldSpec driverName (Nothing :: Maybe [Int])
      ogrFieldSpec driverName ([0,34,76,0,minBound,maxBound] :: U.Vector Int)
      ogrFieldSpec driverName (Just [0,65,maxBound,minBound] :: Maybe (U.Vector Int))
      ogrFieldSpec driverName (Nothing :: Maybe (U.Vector Int))
#else
      ogrFieldSpec driverName (34 :: Int32)
      ogrFieldSpec driverName (Just 65 :: Maybe Int32)
      ogrFieldSpec driverName (Nothing :: Maybe Int32)
      ogrFieldSpec driverName (0 :: Int32)
      ogrFieldSpec driverName (Just 0 :: Maybe Int32)
      ogrFieldSpec driverName (minBound :: Int32)
      ogrFieldSpec driverName (Just minBound :: Maybe Int32)
      ogrFieldSpec driverName (maxBound :: Int32)
      ogrFieldSpec driverName (Just maxBound :: Maybe Int32)
      ogrFieldSpec driverName (34 :: Word32)
      ogrFieldSpec driverName (Just 65 :: Maybe Word32)
      ogrFieldSpec driverName (Nothing :: Maybe Word32)
      ogrFieldSpec driverName (0 :: Int32)
      ogrFieldSpec driverName (Just 0 :: Maybe Word32)
      ogrFieldSpec driverName (minBound :: Word32)
      ogrFieldSpec driverName (Just minBound :: Maybe Word32)
      ogrFieldSpec driverName (maxBound :: Word32)
      ogrFieldSpec driverName (Just maxBound :: Maybe Word32)

      ogrFieldSpec driverName ([0,34,76,0,minBound,maxBound] :: [Int32])
      ogrFieldSpec driverName (Just [0,65,maxBound,minBound] :: Maybe [Int32])
      ogrFieldSpec driverName (Nothing :: Maybe [Int32])
      ogrFieldSpec driverName ([0,34,76,0,minBound,maxBound] :: U.Vector Int32)
      ogrFieldSpec driverName (Just [0,65,maxBound,minBound] :: Maybe (U.Vector Int32))
      ogrFieldSpec driverName (Nothing :: Maybe (U.Vector Int32))
#endif
      ogrFieldSpec driverName (3.4 :: Double)
      ogrFieldSpec driverName (Just 6.5 :: Maybe Double)
      ogrFieldSpec driverName (Nothing :: Maybe Double)
      ogrFieldSpec driverName (3.4 :: Float)
      ogrFieldSpec driverName (Just 6.5 :: Maybe Float)
      ogrFieldSpec driverName (Nothing :: Maybe Float)

      ogrFieldSpec driverName ("foo" :: Text)
      ogrFieldSpec driverName (Just "bar" :: Maybe Text)
      ogrFieldSpec driverName (Nothing :: Maybe Text)
      ogrFieldSpec driverName ("foo" :: String)
      ogrFieldSpec driverName (Just "bar" :: Maybe String)
      ogrFieldSpec driverName (Nothing :: Maybe String)

      ogrFieldSpec driverName ("\x0\x0\x0\0\xDE\xAD\xBE\xEF" :: ByteString)
      ogrFieldSpec driverName (Just "\x0\x0\x0\0\xDE\xAD\xBE\xEF" :: Maybe ByteString)
      ogrFieldSpec driverName (Nothing :: Maybe ByteString)

      ogrFieldSpec driverName ([0,34,76,0] :: [Float])
      ogrFieldSpec driverName (Just [0,65] :: Maybe [Float])
      ogrFieldSpec driverName (Nothing :: Maybe [Float])
      ogrFieldSpec driverName ([0,34,76,0] :: U.Vector Float)
      ogrFieldSpec driverName (Just [0,65] :: Maybe (U.Vector Float))
      ogrFieldSpec driverName (Nothing :: Maybe (U.Vector Float))
      ogrFieldSpec driverName ([0,34,76,0] :: [Double])
      ogrFieldSpec driverName (Just [0,65] :: Maybe [Double])
      ogrFieldSpec driverName (Nothing :: Maybe [Double])
      ogrFieldSpec driverName ([0,34,76,0] :: U.Vector Double)
      ogrFieldSpec driverName (Just [0,65] :: Maybe (U.Vector Double))
      ogrFieldSpec driverName (Nothing :: Maybe (U.Vector Double))

      ogrFieldSpec driverName (["foo","bar"] :: [Text])
      ogrFieldSpec driverName (["foo","bar"] :: V.Vector Text)
      ogrFieldSpec driverName (Just ["bar","foo"] :: Maybe [Text])
      ogrFieldSpec driverName (Nothing :: Maybe [Text])
      ogrFieldSpec driverName (["foo", "bar"] :: [String])
      ogrFieldSpec driverName (["foo", "bar"] :: V.Vector String)
      ogrFieldSpec driverName (Just ["bar", "foo"] :: Maybe [String])
      ogrFieldSpec driverName (Nothing :: Maybe [String])


      ogrFieldSpec driverName (UTCTime (fromGregorian 2010 01 04) 0)
      ogrFieldSpec driverName (UTCTime (fromGregorian 2010 01 04) 5437)

getShapePath :: GDAL s FilePath
getShapePath = liftIO $ getDataFileName "tests/fixtures/fondo.shp"

-- | Makes sure (or tries) that we're not double-freeing, etc by destroying
--   the driver manager after every test and peformimg a major garbage
--   collection to force (really?) the finalizers to run.
setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown =
  before_ OGR.registerAll . after_  performMajorGC . afterAll_ OGR.cleanupAll

strField, realField, intField :: Text -> (Text, FieldDef)
strField  name = (name, FieldDef OFTString  Nothing Nothing Nothing True)
realField name = (name, FieldDef OFTReal    Nothing Nothing Nothing True)
intField  name = (name, FieldDef OFTInteger Nothing Nothing Nothing True)

pointDef :: GeomFieldDef
pointDef = GeomFieldDef WkbPoint Nothing True

data TestFeature a
  = TestFeature  {
      tfGeom :: Geometry
    , tfData :: a
  } deriving (Eq, Show)

instance OGRField a => OGRFeature (TestFeature a) where
  toFeature TestFeature{..} =
    Feature { fFields = [("data", toField tfData)]
            , fGeom   = Just tfGeom
            , fGeoms  = mempty}
  fromFeature Feature{..} = do
    d    <- maybe (Left "TestFeature: field 'data' not found") fromField
                  ("data" `lookupField` fFields)
    geom <- maybe (Left "TestFeature: No Geom") Right fGeom
    return (TestFeature geom d)


instance OGRField a => OGRFeatureDef (TestFeature a) where
  featureDef _ =
    FeatureDef {
      fdName   = "Test"
    , fdFields = [("data", fieldDef (Proxy :: Proxy a))]
    , fdGeom   = GeomFieldDef WkbPoint Nothing True
    , fdGeoms  = mempty}

ogrFieldSpec
  :: forall a. (OGRFeatureDef (TestFeature a), Typeable a, Eq a, Show a)
  => String -> a -> SpecWith (Arg (IO ()))
ogrFieldSpec driverName value = do
  let geom = either exc id (createFromWkt Nothing "POINT (45 87)")
      exc  = error . ("Unexpected createFromWkt error: " ++) . show
      typeName = show (typeOf (undefined :: a))
      feature = TestFeature geom value
      specName = "feature with '" ++ typeName ++ "' field comes out of '" ++
                 driverName ++ "' layer as it got in"

  withDir specName $ \tmpDir -> do
    ds <- create driverName (joinPath [tmpDir, "test"]) []
    r <- try $ do
      l <- createLayer ds StrictOK []
      createFeature l feature >>= getFeature l >>= (`shouldBe` Just feature)
    case r of
      Right () -> return ()
      Left GDALException{gdalErrNum=NotSupported} ->
        -- driver does not support it, oh well...
        warn ("Not supported by driver: " ++ specName)
      Left e  -> throwM e

