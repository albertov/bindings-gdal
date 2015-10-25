{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.OGRSpec (main, spec, setupAndTeardown) where

#include "bindings.h"

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Catch (try, throwM)

import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Int
import Data.Word
import Data.Maybe (isNothing, isJust)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable, typeOf)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import System.Mem (performMajorGC)
import System.FilePath (joinPath)

import GDAL (
    GDAL
  , ErrorNum(..)
  , GDALException(..)
  )
import GDAL.OGR as OGR
import GDAL.OSR (SpatialReference, fromEPSG)

import Paths_bindings_gdal

import TestUtils

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
        check (FeatureDef { fdName   = "Barça Players"
                          , fdFields = mempty
                          , fdGeom   = pointDef {gfdSrs = Just aSrs}
                          , fdGeoms  = mempty})

      when canCreateMultipleGeometryFields $ do
        it "works with several geometry field with no srs" $ do
          check (FeatureDef { fdName   = "Barça Players"
                            , fdFields = mempty
                            , fdGeom   = pointDef
                            , fdGeoms  = [("another_geom", pointDef)]})

        it "works with several geometry field with srs" $ do
          check (FeatureDef { fdName   = "Barça Players"
                            , fdFields = mempty
                            , fdGeom   = pointDef
                            , fdGeoms  = [( "another_geom"
                                          , pointDef {gfdSrs = Just aSrs})]})
    describe "layer CRUD" $ do

      it "can create and retrieve a feature" $ do
        let feat = TestFeature aPoint ("some data" :: String)
        ds <- createMem []
        l <- createLayer ds StrictOK []
        fid <- createFeature l feat
        getFeature l fid >>= (`shouldBe` Just feat)

      it "can create and delete a feature" $ do
        let feat = TestFeature aPoint ("some data" :: String)
        ds <- createMem []
        l <- createLayer ds StrictOK []
        fid <- createFeature l feat
        getFeature l fid >>= (`shouldSatisfy` isJust)
        deleteFeature l fid
        getFeature l fid >>= (`shouldSatisfy` isNothing)

      it "can create and update a feature" $ do
        let feat  = TestFeature aPoint ("some data" :: String)
            feat2 = feat {tfData="other data"}
        ds <- createMem []
        l <- createLayer ds StrictOK []
        fid <- createFeature l feat
        getFeature l fid >>= (`shouldBe` Just feat)
        setFeature l fid feat2
        getFeature l fid >>= (`shouldBe` Just feat2)

      withDir "can retrieve features with less fields than present in layer" $
        \tmpDir -> do
          let path = joinPath [tmpDir, "test.shp"]
              name = "Test"
              someData = "dfsdgfsdgsdf" :: Text
              feat = feature aPoint [ "name"   .= ("Pepe" :: Text)
                                    , "height" .= (187 :: Double)
                                    , "data"   .= someData
                                    ]
              expected = Just (TestFeature aPoint someData)
              fDef =
                FeatureDef {
                  fdName   = name
                , fdFields = [ "name"   `fieldTypedAs` (undefined :: Text)
                             , "height" `fieldTypedAs` (undefined :: Double)
                             , "data"   `fieldTypedAs` (undefined :: String)
                             ]
                , fdGeom   = GeomFieldDef WkbPoint Nothing True
                , fdGeoms  = mempty}
          ds <- create "ESRI Shapefile" path []
          l <- createLayerWithDef ds fDef StrictOK []
          fid <- createFeature l feat
          syncToDisk l
          l2 <- openReadOnly path >>= getLayerByName name
          getFeature l2 fid >>= (`shouldBe` expected)



  describe "getSpatialFilter" $ do

    it "return Noting when no filter has been set" $ do
      mGeom <- getShapePath >>= openReadOnly >>= getLayer 0 >>= getSpatialFilter
      mGeom `shouldSatisfy` isNothing

    it "can set a spatial filter and retrieve it" $ do
      l <- getShapePath >>= openReadWrite >>= getLayer 0
      setSpatialFilter l aPoint
      mGeom <- getSpatialFilter l
      mGeom `shouldBe` Just aPoint


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
      describe ("'"++driverName++"' driver") $ do
#if SUPPORTS_WORD_FIELDS
        ogrFieldSpec driverName (34 :: Int)
        ogrFieldSpec driverName (0 :: Int)
        ogrFieldSpec driverName (minBound :: Int)
        ogrFieldSpec driverName (maxBound :: Int)
        ogrFieldSpec driverName (34 :: Word)
        ogrFieldSpec driverName (0 :: Int)
        ogrFieldSpec driverName (minBound :: Word)
        ogrFieldSpec driverName (maxBound :: Word)

        ogrFieldSpec driverName ([0,34,76,minBound,maxBound] :: [Int])
        ogrFieldSpec driverName ([0,34,76,minBound,maxBound] :: U.Vector Int)
#else
        ogrFieldSpec driverName (34 :: Int32)
        ogrFieldSpec driverName (0 :: Int32)
        ogrFieldSpec driverName (minBound :: Int32)
        ogrFieldSpec driverName (maxBound :: Int32)
        ogrFieldSpec driverName (34 :: Word32)
        ogrFieldSpec driverName (0 :: Int32)
        ogrFieldSpec driverName (minBound :: Word32)
        ogrFieldSpec driverName (maxBound :: Word32)

        ogrFieldSpec driverName ([0,34,76,minBound,maxBound] :: [Int32])
        ogrFieldSpec driverName ([0,34,76,minBound,maxBound] :: U.Vector Int32)
#endif
        ogrFieldSpec driverName (3.4 :: Double)
        ogrFieldSpec driverName (3.4 :: Float)

        ogrFieldSpec driverName ("foo" :: Text)
        ogrFieldSpec driverName ("foo" :: String)

        ogrFieldSpec driverName ("" :: ByteString)
        ogrFieldSpec driverName ("\x0\x0\x0\0\xDE\xCA\xFF" :: ByteString)

        ogrFieldSpec driverName ([0,34,76,0] :: [Float])
        ogrFieldSpec driverName ([0,34,76,0] :: U.Vector Float)
        ogrFieldSpec driverName ([0,34,76,0] :: [Double])
        ogrFieldSpec driverName ([0,34,76,0] :: U.Vector Double)

        ogrFieldSpec driverName (["foo","bar"] :: [Text])
        ogrFieldSpec driverName (["foo","bar"] :: V.Vector Text)
        ogrFieldSpec driverName (["foo", "bar"] :: [String])
        ogrFieldSpec driverName (["foo", "bar"] :: V.Vector String)

        ogrFieldSpec driverName (UTCTime (fromGregorian 2010 01 04) 0)
        ogrFieldSpec driverName (UTCTime (fromGregorian 2010 01 04) 5437)
        ogrFieldSpec driverName
          (LocalTime (fromGregorian 2010 01 04) (TimeOfDay 20 21 34))
        ogrFieldSpec driverName
          (ZonedTime
            (LocalTime (fromGregorian 2010 01 04) (TimeOfDay 21 45 32))
            utc)
        ogrFieldSpec driverName
          (ZonedTime
            (LocalTime (fromGregorian 2010 01 04) (TimeOfDay 22 11 59))
            (minutesToTimeZone 60))
        ogrFieldSpec driverName
          (ZonedTime
            (LocalTime (fromGregorian 2010 01 04) (TimeOfDay 23 59 58))
            (minutesToTimeZone (-60)))
        ogrFieldSpec driverName (fromGregorian 2010 01 04)
        ogrFieldSpec driverName (TimeOfDay 20 00 00)

instance Eq ZonedTime where
  a == b = zonedTimeToUTC a == zonedTimeToUTC b

getShapePath :: GDAL s FilePath
getShapePath = liftIO $ getDataFileName "tests/fixtures/fondo.shp"

setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown = after_ performMajorGC

strField, realField :: Text -> (Text, FieldDef)
strField  name = (name, FieldDef OFTString  Nothing Nothing Nothing True)
realField name = (name, FieldDef OFTReal    Nothing Nothing Nothing True)

pointDef :: GeomFieldDef
pointDef = GeomFieldDef WkbPoint Nothing True

aPoint :: Geometry
aPoint = either exc id (createFromWkt Nothing "POINT (45 87)")
  where exc  = error . ("Unexpected createFromWkt error: " ++) . show

aSrs :: SpatialReference
aSrs = either exc id (fromEPSG 23030)
  where exc = error . ("Unexpected fromEPSG error: " ++) . show

data TestFeature a
  = TestFeature  {
      tfGeom :: Geometry
    , tfData :: a
  } deriving (Eq, Show)

instance OGRField a => OGRFeature (TestFeature a) where
  toFeature TestFeature{..} = feature tfGeom ["data" .= tfData]
  fromFeature f             = TestFeature <$> theGeom f <*> f .: "data"


instance OGRField a => OGRFeatureDef (TestFeature a) where
  featureDef _ =
    FeatureDef {
      fdName   = "Test"
    , fdFields = ["data" `fieldTypedAs` (undefined :: a)]
    , fdGeom   = GeomFieldDef WkbPoint Nothing True
    , fdGeoms  = mempty}

ogrFieldSpec
  :: forall a. ( OGRFeatureDef (TestFeature (Maybe a))
               , OGRFeatureDef (TestFeature a)
               , Typeable a
               , OGRField a
               , Typeable (Maybe a)
               , Eq (Maybe a)
               , Eq a
               , Show (Maybe a)
               , Show a
               )
  => String -> a -> SpecWith (Arg (IO ()))
ogrFieldSpec driverName v = do
  let typeName = show (typeOf (undefined :: a))
      suiteName =
        "feature with '"++typeName++"' field comes out of layer as it got in"
  describe suiteName $ do
    ogrFieldSpec_ v
    ogrFieldSpec_ (Just v)
    ogrFieldSpec_ (Nothing :: Maybe a)

  where
    ogrFieldSpec_ value = do
      let feat = TestFeature aPoint value
          tyName  = show (typeOf value)

      withDir (show value) $ \tmpDir -> do
        ds <- create driverName (joinPath [tmpDir, "test"]) []
        r <- try $ do
          l <- createLayer ds StrictOK []
          createFeature l feat >>= getFeature l >>= (`shouldBe` Just feat)
        case r of
          Right () -> return ()
          Left GDALException{gdalErrNum=NotSupported} ->
            -- driver does not support it, oh well...
            warn ("Not supported by '"++driverName++"' driver: " ++ tyName)
          Left e  -> throwM e

