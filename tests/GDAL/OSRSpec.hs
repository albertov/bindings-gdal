module GDAL.OSRSpec (main, spec) where

import Data.Either (isRight, isLeft)
import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy)

import GDAL.OSR

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can created from EPSG number" $
    fromEPSG 23030 `shouldSatisfy` isRight

  it "can created from Proj4 string" $
    fromProj4 "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      `shouldSatisfy` isRight

  it "fromProj4 returns Left if invalid" $
    fromProj4 "foo" `shouldSatisfy` isLeft

