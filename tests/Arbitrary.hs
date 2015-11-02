{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Arbitrary (InversibleGeotransform(..), positiveXY) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (liftM)

import Test.QuickCheck

import GDAL (XY(XY), Geotransform(Geotransform))
import OGR (Envelope(Envelope))

instance Arbitrary a => Arbitrary (XY a) where
  arbitrary = XY <$> arbitrary <*> arbitrary

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

positiveXY :: Arbitrary (Positive a) => Gen (XY a)
positiveXY = liftM (fmap getPositive) arbitrary

instance (
    Num a
  , Arbitrary a
  , Arbitrary (Positive a)
  ) => Arbitrary (Envelope a) where
  arbitrary = do eMin <- arbitrary
                 sz   <- positiveXY
                 return (Envelope eMin (eMin+sz))
