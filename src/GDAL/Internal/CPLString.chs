{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
module GDAL.Internal.CPLString (
    OptionList
  , HasOptions (..)
  , withOptionList
  , toOptionListPtr
  , peekCPLString
  , fromCPLStringList
  , fromBorrowedCPLStringList
  , withMaybeByteString
  , maybePackCString
  , peekEncodedCString
  , maybePeekEncodedCString
  , useAsEncodedCString
) where

import Control.Exception (bracket, mask_, finally)
import Control.Monad (foldM, void, liftM)

import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Char8 (packCString, useAsCString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Foreign.C.String (CString)
import Foreign.C.Types (CChar(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (lengthArray0, advancePtr)

import GDAL.Internal.CPLConv (cplFinalizerFree, cplFree)

import Lens.Micro (Lens')

#include "cpl_string.h"

type OptionList = [(Text,Text)]

class HasOptions o a | o -> a where
  options :: Lens' o a

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
    peek pptr >>= byteStringFromCPLString >>= maybe (return BS.empty) return

byteStringFromCPLString :: CString -> IO (Maybe ByteString)
byteStringFromCPLString p
  | p==nullPtr = return Nothing
  | otherwise  = do len <- lengthArray0 0 p
                    fp <- newForeignPtr cplFinalizerFree (castPtr p)
                    return $! Just $! PS fp 0 len

fromCPLStringList :: IO (Ptr CString) -> IO [Text]
fromCPLStringList io =
  mask_ $ do
    pList <- io
    if pList==nullPtr
      then return []
      else go pList [] `finally` cplFree pList
  where
    go !pp acc = do
      p <- peek pp
      mS <- maybePeekEncodedCString p `finally` cplFree p
      maybe (return (reverse acc)) (\s -> go (pp `advancePtr` 1) (s:acc)) mS

fromBorrowedCPLStringList :: Ptr CString -> IO [Text]
fromBorrowedCPLStringList ptr
  | ptr==nullPtr = return []
  | otherwise    = go ptr []
  where
    go !p acc = do
      mS <- maybePeekEncodedCString =<< peek p
      maybe (return (reverse acc)) (\s -> go (p `advancePtr` 1) (s:acc)) mS

withMaybeByteString :: Maybe ByteString -> (CString -> IO a) -> IO a
withMaybeByteString Nothing  = ($ nullPtr)
withMaybeByteString (Just s) = useAsCString s

maybePackCString :: CString -> IO (Maybe ByteString)
maybePackCString p
  | p==nullPtr = return Nothing
  | otherwise  = liftM Just (packCString p)

useAsEncodedCString :: Text -> (CString -> IO a) -> IO a
useAsEncodedCString = useAsCString . encodeUtf8

peekEncodedCString :: CString -> IO Text
peekEncodedCString =
  liftM (fromMaybe (error "Unexpected NULL CString in peekEncodedCString")) .
  maybePeekEncodedCString

maybePeekEncodedCString :: CString -> IO (Maybe Text)
maybePeekEncodedCString p
  | p==nullPtr = return Nothing
  | otherwise  = do
      nChars <- lengthArray0 0 p
      bs <- unsafePackCStringLen (p, nChars)
      return $! Just $! decodeUtf8With lenientDecode bs
