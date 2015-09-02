{-# LANGUAGE TemplateHaskell #-}
module OSGeo.Util (
    fromEnumC
  , toEnumC
  , Mutex
  , newMutex
  , withMutex
  , createEnum
) where

import Control.Concurrent (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (finally)
import Foreign.C.Types (CInt)
import Language.Haskell.TH

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral

type Mutex = MVar ()

newMutex :: IO Mutex
newMutex = newMVar ()


acquireMutex :: Mutex -> IO ()
acquireMutex = takeMVar

releaseMutex :: Mutex -> IO ()
releaseMutex m = putMVar m ()

withMutex :: Mutex -> IO a -> IO a
withMutex m action = finally (acquireMutex m >> action) (releaseMutex m)

createEnum :: String -> IO [String] -> Q [Dec]
createEnum name getNames = do
  names <- runIO getNames
  let ctors = map (\n -> NormalC (mkName n) []) names
  return $ [DataD [] (mkName name) [] ctors [''Show, ''Enum, ''Eq, ''Read]]
