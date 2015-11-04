module GDAL.Complex (
    Complex(..)
  , module C
) where

import Data.Complex as C hiding (Complex(..))
import qualified Data.Complex as C

newtype Complex a =
  Complex { unComplex :: C.Complex a }
  deriving (Eq)

instance Show a => Show (Complex a) where
  show (Complex c) = show c
