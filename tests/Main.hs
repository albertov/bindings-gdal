{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.Mem

import qualified Spec

import GDAL
import TestUtils (hspec, setupAndTeardown)

main :: IO ()
main = do
  withGDAL (hspec (setupAndTeardown Spec.spec))
  revertCAFs
  performMajorGC
  performGC

foreign import ccall "revertCAFs" revertCAFs  :: IO ()
