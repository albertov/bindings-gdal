{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils (
    Spec
  , SpecWith
  , Arg
  , shouldSatisfy
  , shouldBe
  , shouldNotBe
  , shouldThrow
  , shouldContain
  , expectationFailure
  , existsAndSizeIsGreaterThan
  , it
  , describe
  , hspec
  , warn
  , withDir
  , after_
  , errorCall
) where

import Control.Monad (guard, unless)
import Control.Monad.Catch (Exception, tryJust, try)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Typeable (typeOf)

import System.IO (IOMode(ReadMode), withBinaryFile, hFileSize)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec (
    Spec
  , SpecWith
  , Arg
  , Selector
  , pendingWith
  , after_
  , errorCall
  )
import qualified Test.Hspec as Hspec

import GDAL (GDAL, GDALException, execGDAL)

hspec :: Spec -> IO ()
hspec = Hspec.hspec . Hspec.parallel

describe :: String -> SpecWith a -> SpecWith a
describe name = Hspec.describe name . Hspec.parallel

it :: String -> (forall s. GDAL s ()) -> SpecWith (Arg (IO ()))
it n a = Hspec.it n (runGDAL' a)

withDir :: String -> (forall s. FilePath -> GDAL s ()) -> SpecWith (Arg (IO ()))
withDir n a =
  Hspec.it n (withSystemTempDirectory "test." (\f -> runGDAL' (a f)))

runGDAL' :: (forall s. GDAL s ()) -> IO ()
runGDAL' a = do
  r <- try (execGDAL a)
  case r of
    Left (e :: GDALException) ->
      Hspec.expectationFailure ("Unexpected GDALException: " ++ show e)
    _ -> return ()

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

shouldContain :: (Show a, Eq a) => [a] -> [a] -> GDAL s ()
shouldContain a  = liftIO . Hspec.shouldContain a

shouldBe :: (Show a, Eq a) => a -> a -> GDAL s ()
shouldBe a  = liftIO . Hspec.shouldBe a

shouldNotBe :: (Show a, Eq a) => a -> a -> GDAL s ()
shouldNotBe a  = liftIO . flip Hspec.shouldSatisfy (/=a)

expectationFailure :: String -> GDAL s ()
expectationFailure = liftIO . Hspec.expectationFailure

warn :: String -> GDAL s ()
warn = liftIO . pendingWith
