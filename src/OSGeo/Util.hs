module OSGeo.Util (
    fromEnumC
  , toEnumC
  , Mutex
  , newMutex
  , withMutex
) where

import Control.Concurrent (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (finally)
import Foreign.C.Types (CInt)

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
