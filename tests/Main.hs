{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Monad (when)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.Mem

import qualified Spec

import GDAL
import GDAL.Internal.GDAL (openDatasetCount)
import TestUtils (hspec)

main :: IO ()
main = withGDAL $ do
  hspec Spec.spec
  revertCAFs
  performGC
  performMajorGC
  dsCount <- openDatasetCount
  when (dsCount>0) $ do
    print ("open datasets at exit:", dsCount)
    exitWith (ExitFailure 1)

foreign import ccall "revertCAFs" revertCAFs  :: IO ()
