module Main where

import qualified Spec
import Control.Exception (finally)

import qualified GDAL as GDAL
import qualified GDAL.OGR as OGR
import TestUtils (hspec)

main :: IO ()
main = do
  GDAL.allRegister
  OGR.registerAll
  hspec Spec.spec
    `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager)
