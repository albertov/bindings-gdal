{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Conduit
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (getArgs)

import GDAL
import OGR

main :: IO ()
main = withGDAL $ execGDAL $ do
  [fname, nameStr] <- liftIO getArgs
  let name = T.pack nameStr
  ds <- OGR.openReadOnly fname
  l <- getLayerByName name ds
  schema <- layerFeatureDef l
  extent <- layerExtent l
  liftIO $ do
    T.putStrLn "Extent:"
    print  extent
    T.putStrLn "Schema:"
    print schema

  runOGR $ sourceLayer (getLayerByName name ds) $$ awaitForever $
    \(mFid, Feature{..}) ->
      liftIO $ do
        T.putStrLn ""
        T.putStrLn ""
        putStrLn ("FID: " <> maybe ("<unknown>") show mFid)
        T.putStrLn "Fields:"
        forM_ (HM.toList fFields) $ \(fieldName, fieldValue) -> do
          T.putStrLn ("  " <> fieldName <> ":")
          putStrLn ("    " <> show fieldValue)
        T.putStrLn ("Geometry:")
        BS.putStrLn (maybe "" (("  "<>) . geomToWkt) fGeom)
