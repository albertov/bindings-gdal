module GDAL.Complex where

import qualified Data.Complex as C

newtype Complex a =
  Complex (C.Complex a)
  deriving (Eq)

instance Show a => Show (Complex a) where
  show (Complex c) = show c

instance Read a => Read (Complex a) where
  read = Complex . read

infix 6 :+
(:+) :: a -> a -> Complex a
a :+ b = Complex (a C.:+ b)

{-
c
Data.Complex.:+         Data.Complex.cis        Data.Complex.imagPart   Data.Complex.mkPolar    Data.Complex.polar
Data.Complex.Complex    Data.Complex.conjugate  Data.Complex.magnitude  Data.Complex.phase      Data.Complex.realPart
-}
