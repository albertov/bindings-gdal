{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDALDriverSpec (main, spec) where

import GDAL
import GDAL.Internal.GDAL ( createDatasetH )
import GDAL.Internal.HSDriver

-- No parallel specs here because things we do here are would affect other
-- tests
import TestUtils hiding (describe, hspec)
import Test.Hspec (hspec)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can register a custom driver" $ do
    d <- createDriver HSDriver
      { hsdName = "MyDriver"
      , hsdIdentify = return . BS.isPrefixOf "foo"
      , hsdOpen = \_ -> do
          d <- driverByName "MEM"
          HsdDatasetH <$> createDatasetH d "MyDesc" (10:+:11) 1 GDT_Byte []
      }
    registerDriver d
    d' <- driverByName "MyDriver"
    description d' >>= (`shouldBe` "MyDriver")
    openReadOnly "bar.tif" GDT_Byte `shouldThrow` ((==OpenFailed) . gdalErrNum)
    ds <- openReadOnly "foo.tif" GDT_Byte
    datasetSize ds `shouldBe` (10:+:11)
    description ds >>= (`shouldBe` "MyDesc")
    deleteDriver d
    driverByName "MyDriver" `shouldThrow` (==UnknownDriver "MyDriver")

  it "survives an exception in the identify callback" $ do
    let info = HSDriver
          { hsdName = "MyDriver"
          , hsdIdentify = error "sheeeeet"
          , hsdOpen = const (return HsdError)
          }
    withDriver info $
     openReadOnly "foo.tif" GDT_Byte
        `shouldThrow` ((==OpenFailed) . gdalErrNum)
