module Main where

import qualified Spec

import GDAL
import TestUtils (hspec)

main :: IO ()
main = withGDAL (hspec Spec.spec)
