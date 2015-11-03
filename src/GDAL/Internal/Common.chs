module GDAL.Internal.Common (
    ApproxOK (..)
) where

#include "gdal.h"

{#enum define ApproxOK
  { TRUE as ApproxOK
  , FALSE as StrictOK
  } deriving (Eq, Show) #}
