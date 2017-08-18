{-# LANGUAGE CPP #-}
import Control.Applicative
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



#if !MIN_VERSION_Cabal(2,0,0)
mkFlagName = FlagName
#endif

main = defaultMainWithHooks simpleUserHooks {confHook = gdalConf}

gdalConf (pkg0, pbi) flags = do
 lbi <- confHook simpleUserHooks (pkg0, pbi) flags
 case mkFlagName "autoconfig" `lookup` configConfigurationsFlags flags of
   Just False -> putStrLn "NOT using gdal-config" >> return lbi
   _          -> putStrLn "Using gdal-config" >> configureWithGdalConfig lbi flags

configureWithGdalConfig lbi flags = do
 gdalInclude <- getFlagValues 'I' <$> gdalConfig ["--cflags"]
 let isStatic = maybe False id $
                mkFlagName "static" `lookup` configConfigurationsFlags flags
     getLibArgs = gdalConfig ["--libs"]
                : if isStatic then [gdalConfig ["--dep-libs"]] else []
 libArgs <- intercalate " "  <$> sequence  getLibArgs
 let gdalLibDirs = getFlagValues 'L' libArgs
     (gdalLibs, staticDirs) = unzip . parseLibraries . words $ libArgs
     hasPg    = any (=="pq"    ) gdalLibs
     hasCurl  = any (=="curl"  ) gdalLibs
     hasGeos  = any (=="geos_c") gdalLibs
     gdalLibsStatic = (if hasGeos then (++["geos"]) else id)
              -- assumes curl or pg are compile with ssl support
              . (if hasCurl || hasPg then (++["ssl","crypto"]) else id) 
              $ gdalLibs
     updBinfo bi = bi { extraLibDirs = extraLibDirs bi
                                    ++ gdalLibDirs
                                    ++ catMaybes staticDirs
                      , extraLibs    = extraLibs bi
                                    ++ (if isStatic then gdalLibsStatic else gdalLibs)
                      , includeDirs  = includeDirs  bi ++ gdalInclude
                      }
     -- | appendGeos: makes sure 'geos_c' is included before 'geos' so symbols
     --   can be resolved when linking statically
     appendGeos [] = []
     appendGeos ("geos_c":xs) = "geos_c" : "geos" : xs
     appendGeos (x:xs)        = x : appendGeos xs
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
  cmd <- maybe (liftM init (getOutput "bash" ["-c", "which gdal-config"])) return mCmd
  rstrip '\n' <$> getOutput "bash" (cmd:args)

rstrip :: Char -> String -> String
rstrip c = reverse . dropWhile (==c) . reverse

parseLibraries :: [String] -> [(String, Maybe FilePath)]
parseLibraries = concatMap go
  where
    go ('-':'l':name) = [(name, Nothing)]
    go ('-':_)        = []
    go p              = case staticLibNameAndPath p of
                          Just (n,p)  -> [(n,Just p)]
                          Nothing     -> []

getFlagValues f s = map (\(_:_:v) -> v) filtered
  where filtered = filter (\(_:f':_) -> f==f') (words . rstrip $ s)
        rstrip = reverse . rlstrip . reverse
        rlstrip ('\n':'\r':x) = x
        rlstrip ('\n':x) = x
        rlstrip x = x
 
staticLibNameAndPath :: FilePath -> Maybe (String, FilePath)
staticLibNameAndPath p
  | takeExtension p == staticLibSuffix
  , staticLibPrefix `isPrefixOf` takeBaseName p
  = Just (drop (length staticLibPrefix) (takeBaseName p), takeDirectory p)
staticLibNameAndPath _ = Nothing

-- FIXME: make portable
staticLibPrefix, staticLibSuffix :: String
staticLibPrefix = "lib"
staticLibSuffix = ".a"
