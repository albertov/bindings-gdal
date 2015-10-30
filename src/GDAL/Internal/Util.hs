{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.Util (
    fromEnumC
  , toEnumC
  , createEnum
  , useAsEncodedCString
  , peekEncodedCString
) where

import Data.ByteString.Char8 (useAsCString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Storable (peekElemOff)
import Language.Haskell.TH

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum
{-# INLINE[0] fromEnumC #-}

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral
{-# INLINE[0] toEnumC #-}

{-# RULES
"toEnumC/fromEnumC" forall a. toEnumC (fromEnumC a) = a
"fromEnumC/toEnumC" forall a. fromEnumC (toEnumC a) = a
  #-}

createEnum :: String -> IO [String] -> Q [Dec]
createEnum name getNames = do
  names <- runIO getNames
  let ctors = map (\n -> NormalC (mkName n) []) names
  return $ [DataD [] (mkName name) [] ctors [''Show, ''Enum, ''Eq, ''Read]]

useAsEncodedCString :: Text -> (CString -> IO a) -> IO a
useAsEncodedCString = useAsCString . encodeUtf8

peekEncodedCString :: CString -> IO Text
peekEncodedCString p = do
  nChars <- len 0
  bs <- unsafePackCStringLen (p, nChars)
  return $! decodeUtf8With lenientDecode bs
  where
    len !n = peekElemOff p n >>= (\v -> if v==0 then return n else len (n+1))
