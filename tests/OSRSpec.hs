{-# LANGUAGE OverloadedStrings #-}
module OSRSpec (main, spec) where

import Data.Either (isRight, isLeft)
import TestUtils

import OSR

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can created from EPSG number" $
    srsFromEPSG 23030 `shouldSatisfy` isRight

  it "can created from Proj4 string" $
    srsFromProj4 "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      `shouldSatisfy` isRight

  it "srsFromProj4 returns Left if invalid" $
    srsFromProj4 "foo" `shouldSatisfy` isLeft

