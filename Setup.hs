import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Exit
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks {confHook = gdalConf}


gdalConf (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 gdalInclude <- getIncludeDirs
 gdalLibDirs <- getExtraLibDirs
 gdalLibs    <- getExtraLibs
 let lpd        = localPkgDescr lbi
     lib        = fromJust (library lpd)
     libbi      = libBuildInfo lib
     libbi'     = libbi {
                     extraLibDirs = extraLibDirs libbi ++ gdalLibDirs
                   , extraLibs    = extraLibs    libbi ++ gdalLibs
                   , includeDirs  = includeDirs  libbi ++ gdalInclude
                   }
     lib'       = lib { libBuildInfo = libbi' }
     lpd'       = lpd { library = Just lib' }
 return $ lbi { localPkgDescr = lpd' }

getOutput s a = readProcess s a ""

getExtraLibs = liftM (getFlagValues 'l') $ getOutput "gdal-config" ["--libs"]

getExtraLibDirs = liftM (getFlagValues 'L') $ getOutput "gdal-config" ["--libs"]

getIncludeDirs = liftM (getFlagValues 'I') $ getOutput "gdal-config" ["--cflags"]

getFlagValues f s = map (\(_:_:v) -> v) filtered
  where filtered = filter (\(_:f':_) -> f==f') (words . init $ s)
