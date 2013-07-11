{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import Control.Applicative
import Control.Exception (handle, SomeException)
import Data.Maybe (fromJust, isJust)

import System.IO
import System.IO.Temp
import System.FilePath

import Test.Framework.TH
-- import Test.QuickCheck
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit


import Bindings.GDAL.Internal

main :: IO ()
main = $(defaultMainGenerator)

case_can_create :: IO ()
case_can_create = withSystemTempDirectory "test." $ \tmpDir -> do
    let p = joinPath [tmpDir, "test.tif"]
        d = driverByName "GTIFF"
    assertBool "Could not load Gtiff driver" (isJust d)
    ds <- create (fromJust d) p 3000 3000 1 GDT_Int16
          [("compress","deflate"), ("zlevel", "9"), ("predictor", "2")]
    assertBool "Could not create dataset" (isJust ds)
    flushCache (fromJust ds)
    assertExistsAndSizeGreaterThan p 20000

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize p
    = handle (\(_ :: SomeException) -> return Nothing) $
        withBinaryFile p ReadMode $ \h -> Just <$> hFileSize h

assertExistsAndSizeGreaterThan :: FilePath -> Integer -> IO ()
assertExistsAndSizeGreaterThan p s = do
    size <- getFileSize p
    assertBool ("file " ++ show p ++ " was not created") (isJust size)
    let s2 = fromJust size
    assertBool ("File size of " ++ show p ++ ", " ++ show s2 ++ " is <= " ++
                show s)
               (s2 > s)
