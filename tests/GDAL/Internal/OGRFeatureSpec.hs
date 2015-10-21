{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAL.Internal.OGRFeatureSpec (main, spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Test.Hspec (Spec, hspec, describe)

import GDAL.OGRSpec (setupAndTeardown)
import GDAL.Internal.OGRFeature


import TestUtils (
    shouldThrow
  , shouldBe
  , shouldNotBe
  , shouldContain
  , shouldSatisfy
  , it
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "withFieldDefnH / fieldDefFromHandle" $ do
    let checkIso f =
         liftIO (withFieldDefnH f fieldDefFromHandle) >>= (`shouldBe` f)
        strFieldDef  = fieldDef OFTString
        realFieldDef = fieldDef OFTReal

    it "handles blank names" $ checkIso (strFieldDef "")

    it "handles unicode names" $ checkIso (strFieldDef "Ñçö^fooç")

    it "handles width on str field" $
      checkIso ((strFieldDef "Ñçö^fooç") {fldWidth=Just 5})

    it "handles width on real field" $
      checkIso ((realFieldDef "Ñçö^fooç") {fldWidth=Just 5})

    it "handles precision on str field" $
      checkIso ((strFieldDef "Ñçö^fooç") {fldPrec=Just 5})

    it "handles precision on real field" $
      checkIso ((realFieldDef "Ñçö^fooç") {fldPrec=Just 5})

    it "handles JustifyLeft on str field" $
      checkIso ((strFieldDef "Ñçö^fooç") {fldJust=Just JustifyLeft})

    it "handles JustifyRight on str field" $
      checkIso ((strFieldDef "Ñçö^fooç") {fldJust=Just JustifyRight})

