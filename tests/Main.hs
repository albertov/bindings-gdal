{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import System.Mem

import qualified Spec

import GDAL
import TestUtils (hspec)

main :: IO ()
main = withGDAL $ do
  hspec Spec.spec
  revertCAFs
  performGC
  performMajorGC

foreign import ccall "revertCAFs" revertCAFs  :: IO ()
