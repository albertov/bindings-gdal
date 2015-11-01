module Main where

import qualified Spec

import GDAL
import TestUtils (hspec, setupAndTeardown)

main :: IO ()
main = withGDAL (hspec (setupAndTeardown Spec.spec))
