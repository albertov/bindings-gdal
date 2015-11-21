{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Arbitrary (InversibleGeotransform(..), positivePair) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM)

import Test.QuickCheck

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
             <$> arbitrary
             <*> liftM getNonZero arbitrary
             <*> pure 0
             <*> arbitrary
             <*> pure 0
             <*> liftM getNonZero arbitrary)

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
