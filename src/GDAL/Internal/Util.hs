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
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Foreign (peekCStringLen)

import Foreign.C.Types (CInt)
import Foreign.C.String (CString)
import Foreign.Storable (peekElemOff)
import Language.Haskell.TH

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum
{-# INLINE fromEnumC #-}

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral
{-# INLINE toEnumC #-}

createEnum :: String -> IO [String] -> Q [Dec]
createEnum name getNames = do
  names <- runIO getNames
  let ctors = map (\n -> NormalC (mkName n) []) names
  return $ [DataD [] (mkName name) [] ctors [''Show, ''Enum, ''Eq, ''Read]]

useAsEncodedCString :: Text -> (CString -> IO a) -> IO a
useAsEncodedCString = useAsCString . encodeUtf8

peekEncodedCString :: CString -> IO Text
peekEncodedCString p = len 0 >>= (\l -> peekCStringLen (p,l))
  where
    len !n = peekElemOff p n >>= (\v -> if v==0 then return n else len (n+1))
