{-# LANGUAGE TemplateHaskell #-}
module GDAL.Internal.Util (
    Mutex
  , newMutex
  , withMutex
  , fromEnumC
  , toEnumC
  , createEnum
) where

import Control.Concurrent (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (finally)
import Foreign.C.Types (CInt)
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

newtype Mutex = Mutex (MVar ())

newMutex :: IO Mutex
newMutex = fmap Mutex (newMVar ())

withMutex :: Mutex -> IO a -> IO a
withMutex (Mutex m) action = finally (acquireMutex >> action) releaseMutex
  where
    acquireMutex = takeMVar m
    releaseMutex = putMVar m ()
