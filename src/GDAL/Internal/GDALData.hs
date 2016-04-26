{-# LANGUAGE TemplateHaskell #-}

module GDAL.Internal.GDALData (
    ensureExists
  , files
  , unpackAt
) where

import           GDAL.Internal.GDAL (version)
import           GDAL.Internal.CPLConv (setConfigOption)
import           Control.Monad (liftM, when, forM_)
import qualified Data.ByteString.Char8 as BS
import           Data.FileEmbed (embedDir)
import           Language.Haskell.TH (runIO)
import           System.Environment (lookupEnv, setEnv)
import           System.FilePath.Posix
import           System.Process (readProcess)
import           System.Directory ( getAppUserDataDirectory
                                  , createDirectoryIfMissing
                                  , doesDirectoryExist
                                  )


files :: [(FilePath, BS.ByteString)]
files = $(do
  dataDir <- runIO $ do
    mEnv <- lookupEnv "GDAL_DATA"
    case mEnv of
      Just d -> return d
      Nothing -> liftM init (readProcess "gdal-config" ["--datadir"] "")
  embedDir dataDir
  )

unpackAt :: FilePath -> IO ()
unpackAt dest =
  forM_ files $ \(p, d) -> do
    let destFile = dest </> p
    createDirectoryIfMissing True (takeDirectory destFile)
    BS.writeFile destFile d

ensureExists :: IO ()
ensureExists = do
  fromEnv <- lookupEnv "GDAL_DATA"
  case fromEnv of
    Just _ -> return ()
    Nothing -> do
      userDataDir <- getAppUserDataDirectory "bindings-gdal"
      let dataDir = userDataDir </> "data" </> verStr
          (a,b)   = version
          verStr  = show a ++ "." ++ show b
      exists <- doesDirectoryExist dataDir
      when (not exists) (unpackAt dataDir)
      setEnv "GDAL_DATA" dataDir
      setConfigOption (BS.pack "GDAL_DATA") (BS.pack dataDir)
