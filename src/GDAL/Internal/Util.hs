{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.Util (
    fromEnumC
  , toEnumC
  , createEnum
) where

import Foreign.C.Types (CInt)
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
