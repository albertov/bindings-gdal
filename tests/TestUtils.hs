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
  , itIO
  , describe
  , hspec
  , warn
  , pendingWith
  , withDir
  , after_
  , errorCall
  , setupAndTeardown
) where

import Control.Monad (guard, unless)
import Control.Monad.Catch (Exception, MonadCatch, tryJust, try)
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
  , after_
  , errorCall
  )
import qualified Test.Hspec as Hspec

import GDAL (GDAL, GDALException, runGDAL)

hspec :: Spec -> IO ()
hspec = Hspec.hspec . Hspec.parallel

describe :: String -> SpecWith (Arg (IO ())) -> SpecWith (Arg (IO ()))
describe name = Hspec.describe name . Hspec.parallel

it :: String -> (forall s. GDAL s ()) -> SpecWith (Arg (IO ()))
it n a = Hspec.it n (runGDAL' a)

-- For things that we want to make sure that can run outside of the GDAL monad
itIO :: String -> IO () -> SpecWith (Arg (IO ()))
itIO n = Hspec.it n

withDir :: String -> (forall s. FilePath -> GDAL s ()) -> SpecWith (Arg (IO ()))
withDir n a =
  Hspec.it n (withSystemTempDirectory "test." (\f -> runGDAL' (a f)))

runGDAL' :: (forall s. GDAL s ()) -> IO ()
runGDAL' a = do
  r <- try (runGDAL a)
  case r of
    Left (e :: GDALException) ->
      Hspec.expectationFailure ("Unexpected GDALException: " ++ show e)
    Right () -> return ()

existsAndSizeIsGreaterThan
  :: (MonadIO m, MonadCatch m)
  => FilePath -> Integer -> m ()
existsAndSizeIsGreaterThan p s = do
  r <- tryJust (guard . isDoesNotExistError)
               (liftIO (withBinaryFile p ReadMode hFileSize))
  case r of
    Left _   -> expectationFailure "File was not created"
    Right sz -> sz `shouldSatisfy` (>s)

shouldThrow
  :: (MonadCatch m, MonadIO m)
  => Exception e => m a -> Selector e -> m ()
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

shouldSatisfy :: (MonadIO m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy a = liftIO . Hspec.shouldSatisfy a

shouldContain :: (MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain a  = liftIO . Hspec.shouldContain a

shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe a  = liftIO . Hspec.shouldBe a

shouldNotBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldNotBe a  = liftIO . flip Hspec.shouldSatisfy (/=a)

expectationFailure :: MonadIO m => String -> m ()
expectationFailure = liftIO . Hspec.expectationFailure

pendingWith, warn :: MonadIO m => String -> m ()
pendingWith = liftIO . Hspec.pendingWith
warn = pendingWith

setupAndTeardown :: SpecWith a -> SpecWith a
setupAndTeardown = id
