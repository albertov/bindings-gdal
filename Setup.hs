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
 gdalInclude <- liftM (getFlagValues 'I') $ getOutput "gdal-config" ["--cflags"]
 gdalLibDirs <- liftM (getFlagValues 'L') $ getOutput "gdal-config" ["--libs"]
 gdalLibs    <- liftM (getFlagValues 'l') $ getOutput "gdal-config" ["--libs"]
 gdalVers    <- getOutput "gdal-config" ["--version"]
 let (vMajor,r) = break (=='.') gdalVers
     (vMinor,_) = break (=='.') (tail r)
     updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ gdalLibDirs
                      , extraLibs    = extraLibs    bi ++ gdalLibs
                      , includeDirs  = includeDirs  bi ++ gdalInclude
                      , cppOptions   = cppOptions   bi ++
                                         [ "-DGDAL_VERSION_MAJOR=" ++ vMajor
                                         , "-DGDAL_VERSION_MINOR=" ++ vMinor]
                      }
     updLib lib = lib { libBuildInfo  = updBinfo (libBuildInfo lib)}
     updTs  ts  = ts  { testBuildInfo = updBinfo (testBuildInfo ts)}
     updBm  bm  = bm  { benchmarkBuildInfo = updBinfo (benchmarkBuildInfo bm)}
     updExe ex  = ex  { buildInfo     = updBinfo (buildInfo ex)}
     updLpd lpd = lpd { library       = fmap updLib (library lpd)
                      , testSuites    = map updTs (testSuites lpd)
                      , benchmarks    = map updBm (benchmarks lpd)
                      , executables   = map updExe (executables lpd)
                      }
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 return (lbi { localPkgDescr = updLpd (localPkgDescr lbi) })

getOutput s a = readProcess s a ""

getFlagValues f s = map (\(_:_:v) -> v) filtered
  where filtered = filter (\(_:f':_) -> f==f') (words . init $ s)
