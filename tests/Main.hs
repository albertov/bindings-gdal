module Main where

import qualified Spec
import Test.Hspec.Runner
import Control.Exception (finally)

import qualified GDAL as GDAL
import qualified GDAL.OGR as OGR

main :: IO ()
main = do
  GDAL.allRegister
  OGR.registerAll
  hspecWith defaultConfig Spec.spec
    `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager)
