{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Arbitrary (
    InversibleGeotransform(..)
  , positivePair
  , (~==)
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM)
import Data.Fixed (Fixed, E3)

import Test.QuickCheck hiding (Fixed)

import GDAL (Pair(..), Geotransform(Geotransform))
import OGR (Envelope(Envelope))

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = (:+:) <$> arbitrary <*> arbitrary

newtype InversibleGeotransform =
  InversibleGeotransform { getInversible :: Geotransform }
  deriving (Eq, Show)

instance Arbitrary InversibleGeotransform where
  arbitrary =
    InversibleGeotransform
      <$> (Geotransform
             <$> liftM getFixed        arbitrary
             <*> liftM getFixedNonZero arbitrary
             <*> pure 0
             <*> liftM getFixed        arbitrary
             <*> pure 0
             <*> liftM getFixedNonZero arbitrary)
    where
      getFixed :: Fixed E3 -> Double
      getFixed = realToFrac
      getFixedNonZero = getFixed . getNonZero

infix 4 ~==
(~==) :: (Fractional a, Ord a) => a -> a -> Bool
a ~== b = abs(a-b)<epsilon
  where epsilon = 1e-2

instance Arbitrary Geotransform where
  arbitrary =
    Geotransform
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

positivePair :: Arbitrary (Positive a) => Gen (Pair a)
positivePair = liftM (fmap getPositive) arbitrary

instance (
    Num a
  , Arbitrary a
  , Arbitrary (Positive a)
  ) => Arbitrary (Envelope a) where
  arbitrary = do eMin <- arbitrary
                 sz   <- positivePair
                 return (Envelope eMin (eMin+sz))
