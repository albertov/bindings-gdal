module GDAL.Internal.Common (
    ApproxOK (..)
  , true
  , false
) where

#include "gdal.h"

import Foreign.C.Types

{#enum define ApproxOK
  { TRUE as ApproxOK
  , FALSE as StrictOK
  } deriving (Eq, Show) #}

true, false :: CInt
true = {#const TRUE #}
false = {#const FALSE #}
