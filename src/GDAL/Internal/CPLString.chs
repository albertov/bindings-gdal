{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.CPLString (
    OptionList
  , withOptionList
  , toOptionListPtr
  , peekCPLString
  , fromCPLStringList
  , fromBorrowedCPLStringList
) where

import Control.Exception (bracket, mask_, finally)
import Control.Monad (foldM, void, liftM, (>=>))

import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Char8 (packCString)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid (mempty)
import Data.Text (Text)

import Foreign.C.String (CString)
import Foreign.C.Types (CChar(..), CInt(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (lengthArray0, advancePtr)

import GDAL.Internal.Util (useAsEncodedCString)
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

peekCPLString :: (Ptr CString -> IO a) -> IO ByteString
peekCPLString act =
  mask_ $
  with nullPtr $ \pptr -> do
    void (act pptr)
    peek pptr >>= byteStringFromCPLString >>= maybe (return mempty) return

byteStringFromCPLString :: CString -> IO (Maybe ByteString)
byteStringFromCPLString p
  | p==nullPtr = return Nothing
  | otherwise  = do len <- lengthArray0 0 p
                    fp <- newForeignPtr c_cplFree (castPtr p)
                    return $! Just $! PS fp 0 len

fromCPLStringList :: IO (Ptr CString) -> IO [ByteString]
fromCPLStringList io =
  mask_ $ do
    pList <- io
    if pList==nullPtr
      then return []
      else go pList [] `finally` cplFree pList
  where
    go !pp acc = do
      mS <- byteStringFromCPLString =<< peek pp
      maybe (return (reverse acc)) (\s -> go (pp `advancePtr` 1) (s:acc)) mS

fromBorrowedCPLStringList :: Ptr CString -> IO [(ByteString,ByteString)]
fromBorrowedCPLStringList ptr = do
  n <- liftM fromIntegral ({#call unsafe CSLCount as ^#} ptr)
  mapM (peekElemOff ptr >=> liftM splitIt . packCString) [0..n-1]
  where
    splitIt s = case BS.break (=='=') s of
                  (k,"") -> (k,"")
                  (k,v)  -> (k,BS.tail v)
