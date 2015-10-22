{-# LANGUAGE ForeignFunctionInterface #-}
module GDAL.Internal.CPLString (
    OptionList
  , withOptionList
  , toOptionListPtr
  , fromOptionListPtr
) where

import Control.Exception (bracket)
import Control.Monad (forM, foldM)

import Data.Text (Text)
import qualified Data.Text as T

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import GDAL.Internal.Util (useAsEncodedCString, peekEncodedCString)

#include "cpl_string.h"

type OptionList = [(Text,Text)]

withOptionList :: OptionList -> (Ptr CString -> IO c) -> IO c
withOptionList opts = bracket (toOptionListPtr opts) freeOptionList
  where freeOptionList = {#call unsafe CSLDestroy as ^#} . castPtr

toOptionListPtr :: OptionList -> IO (Ptr CString)
toOptionListPtr = foldM folder nullPtr
  where
    folder acc (k,v) =
      useAsEncodedCString k $ \k' ->
      useAsEncodedCString v $ \v' ->
        {#call unsafe CSLSetNameValue as ^#} acc k' v'

fromOptionListPtr :: Ptr CString -> IO OptionList
fromOptionListPtr ptr = do
  n <- {#call unsafe CSLCount as ^#} ptr
  forM [0..n-1] $ \ix -> do
    s <- {#call unsafe CSLGetField as ^#} ptr ix >>= peekEncodedCString
    return $ T.break (/='=') s
