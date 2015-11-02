{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.CPLString (
    OptionList
  , withOptionList
  , toOptionListPtr
  , fromOptionListPtr
  , peekCPLString
) where

import Control.Exception (bracket, bracketOnError)
import Control.Monad (forM, foldM, liftM, when, void)

import Data.ByteString.Internal (ByteString(..))
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (lengthArray0)

import GDAL.Internal.Util (useAsEncodedCString, peekEncodedCString)
import GDAL.Internal.CPLConv (c_cplFree, cplFree)

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

peekCPLString :: (Ptr CString -> IO a) -> IO ByteString
peekCPLString act = with nullPtr $ \pptr ->
  bracketOnError (go pptr) (const (freeIfNotNull pptr)) return
  where
    go pptr = do
      void (act pptr)
      p <- liftM castPtr (peek pptr) :: IO (Ptr Word8)
      if p /= nullPtr
        then do len <- lengthArray0 0 p
                fp <- newForeignPtr c_cplFree p
                return $! PS fp 0 len
        else return mempty

    freeIfNotNull pptr = do
      p <- peek pptr
      when (p /= nullPtr) (cplFree p)
