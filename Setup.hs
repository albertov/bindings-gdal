import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Exit
import Data.List
import Distribution.Simple
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks {confHook = gdalConf}

gdalConf (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 case FlagName "autoconfig" `lookup` configConfigurationsFlags flags of
   Just False -> putStrLn "NOT using gdal-config" >> return lbi
   _          -> putStrLn "Using gdal-config" >> configureWithGdalConfig lbi

configureWithGdalConfig lbi = do
 gdalInclude <- liftM (getFlagValues 'I') $ getOutput "gdal-config" ["--cflags"]
 gdalLibDirs <- liftM (getFlagValues 'L') $ getOutput "gdal-config" ["--libs"]
 gdalLibs    <- liftM (getFlagValues 'l') $ getOutput "gdal-config" ["--libs"]
 let updBinfo bi = bi { extraLibDirs = extraLibDirs bi ++ gdalLibDirs
                      , extraLibs    = extraLibs    bi ++ gdalLibs
                      , includeDirs  = includeDirs  bi ++ gdalInclude
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
 return (lbi { localPkgDescr = updLpd (localPkgDescr lbi) })

getOutput s a = readProcess s a ""

getFlagValues f s = map (\(_:_:v) -> v) filtered
  where filtered = filter (\(_:f':_) -> f==f') (words . init $ s)
