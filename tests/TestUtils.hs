{-# LANGUAGE RankNTypes #-}
module TestUtils (
    shouldSatisfy
  , shouldBe
  , shouldThrow
  , expectationFailure
  , existsAndSizeIsGreaterThan
  , it
  , withDir
) where

import Control.Monad (guard, unless)
import Control.Monad.Catch (Exception, tryJust, try)
import Control.Monad.IO.Class (liftIO)

import Data.Typeable (typeOf)

import System.IO (IOMode(ReadMode), withBinaryFile, hFileSize)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec (SpecWith, Arg, Selector)
import qualified Test.Hspec as Hspec

import GDAL (GDAL, runGDAL)

it :: String -> (forall s. GDAL s ()) -> SpecWith (Arg (IO ()))
it n a = Hspec.it n (runGDAL a)

withDir :: String -> (forall s. FilePath -> GDAL s ()) -> SpecWith (Arg (IO ()))
withDir n a = Hspec.it n (withSystemTempDirectory "test." (\f -> runGDAL (a f)))


existsAndSizeIsGreaterThan :: FilePath -> Integer -> GDAL s ()
existsAndSizeIsGreaterThan p s = do
  r <- tryJust (guard . isDoesNotExistError)
               (liftIO (withBinaryFile p ReadMode hFileSize))
  case r of
    Left _   -> expectationFailure "File was not created"
    Right sz -> sz `shouldSatisfy` (>s)

shouldThrow :: Exception e => GDAL s a -> Selector e -> GDAL s ()
action `shouldThrow` p = do
  r <- try action
  case r of
    Right _ ->
      expectationFailure $
        "did not get expected exception: " ++ exceptionType
    Left e -> unless (p e) $ expectationFailure $
                "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "OSGeo.TestUtils.shouldThrow: broken Typeable instance"

shouldSatisfy :: Show a => a -> (a -> Bool) -> GDAL s ()
shouldSatisfy a = liftIO . Hspec.shouldSatisfy a

shouldBe :: (Show a, Eq a) => a -> a -> GDAL s ()
shouldBe a  = liftIO . Hspec.shouldBe a

expectationFailure :: String -> GDAL s ()
expectationFailure = liftIO . Hspec.expectationFailure

