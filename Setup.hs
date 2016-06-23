import Control.Monad
import Data.Maybe
import System.Process
import System.IO
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath.Posix
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
 gdalInclude <- liftM (getFlagValues 'I') $ gdalConfig ["--cflags"]
 gdalLibDirs <- liftM (getFlagValues 'L') $ gdalConfig ["--libs", "--dep-libs"]
 (gdalLibs, staticDirs) <- liftM (unzip . parseLibraries . words)
                            (gdalConfig ["--libs", "--dep-libs"])
 let updBinfo bi = bi { extraLibDirs = extraLibDirs bi
                                    ++ gdalLibDirs
                                    ++ catMaybes staticDirs
                      , extraLibs    = extraLibs bi
                                    ++ gdalLibs
                                    ++ if hasStaticLibs then [stdCpp] else []
                      , includeDirs  = includeDirs  bi ++ gdalInclude
                      }
     hasStaticLibs = not (null (catMaybes staticDirs))
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

gdalConfig args = do
  mCmd <- lookupEnv "GDAL_CONFIG"
  cmd <- maybe (getOutput "bash" ["-c", "which gdal-config"]) return mCmd
  getOutput "bash" (cmd:args)

parseLibraries :: [String] -> [(String, Maybe FilePath)]
parseLibraries = concatMap go
  where
    go ('-':'l':name) = [(name, Nothing)]
    go ('-':_)        = []
    go p              = case staticLibNameAndPath p of
                          Just (n,p)  -> [(n,Just p)]
                          Nothing     -> []

getFlagValues f s = map (\(_:_:v) -> v) filtered
  where filtered = filter (\(_:f':_) -> f==f') (words . init $ s)
 
staticLibNameAndPath :: FilePath -> Maybe (String, FilePath)
staticLibNameAndPath p
  | takeExtension p == staticLibSuffix
  , staticLibPrefix `isPrefixOf` takeBaseName p
  = Just (drop (length staticLibPrefix) (takeBaseName p), takeDirectory p)
staticLibNameAndPath _ = Nothing

-- FIXME: make portable
staticLibPrefix, staticLibSuffix, stdCpp :: String
staticLibPrefix = "lib"
staticLibSuffix = ".a"
stdCpp = "stdc++"
