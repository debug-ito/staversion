-- |
-- Module: Staversion.Internal.Cabal
-- Description: functions dealing with .cabal files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Cabal
       ( loadCabalFile,
         Target(..),
         BuildDepends(..)
       ) where

import Data.Text (pack)
import Distribution.Package (Dependency(..), unPackageName)
import Distribution.PackageDescription
  (BuildInfo(targetBuildDepends), Library(libBuildInfo), Executable(buildInfo, exeName),
   TestSuite(testName, testBuildInfo), Benchmark(benchmarkName, benchmarkBuildInfo),
   PackageDescription(library, executables, testSuites, benchmarks)
  )
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.Configuration
  (finalizePackageDescription)
import Distribution.System (buildPlatform)
import Distribution.Compiler (unknownCompilerInfo, buildCompilerId, AbiTag(NoAbiTag))

import Staversion.Internal.Query
  ( PackageName, ErrorMsg
  )

-- | Build target type.
data Target = TargetLibrary -- ^ the @library@ target.
            | TargetExecutable String -- ^ the @executable NAME@ target.
            | TargetTestSuite String -- ^ the @test-suite NAME@ target.
            | TargetBenchmark String -- ^ the @benchmark NAME@ target.
            deriving (Show,Eq,Ord)

-- | A block of @build-depends:@.
data BuildDepends =
  BuildDepends { depsTarget :: Target,
                 depsPackages :: [PackageName]
               } deriving (Show,Eq,Ord)

loadCabalFile :: FilePath -> IO (Either ErrorMsg [BuildDepends])
loadCabalFile cabal_filepath = impl where
  impl = do
    -- TODO: catch IO exception.
    cabal_file_content <- readFileStrict cabal_filepath
    return $ fmap toBuildDependsList $ resolveCond =<< (toEither $ parsePackageDescription cabal_file_content)
  readFileStrict file = (\s -> length s `seq` return s) =<< readFile file
  toEither (ParseFailed e) = Left ("Failed to parse " ++ cabal_filepath ++ ": " ++ show e)
  toEither (ParseOk _ ret) = Right ret
  resolveCond = either makeDepsError (Right . fst) . finalizePackageDescription [] (const True) buildPlatform my_compiler_info []
  my_compiler_info = unknownCompilerInfo buildCompilerId NoAbiTag -- is this OK??
  makeDepsError deps = Left ("Unexpected fatal error: it claims the following dependencies are not met: " ++ show deps)
  toBuildDependsList pdesc = libs ++ exes ++ tests ++ benches where
    libs = maybe [] return $ fmap depsLibrary $ library pdesc
    exes = map depsExecutable $ executables pdesc
    tests = map depsTestSuite $ testSuites pdesc
    benches = map depsBenchmark $ benchmarks pdesc

depsBuildInfo :: BuildInfo -> [PackageName]
depsBuildInfo = map toName .targetBuildDepends where
  toName (Dependency pname _) = pack $ unPackageName pname

depsLibrary :: Library -> BuildDepends
depsLibrary lib = BuildDepends { depsTarget = TargetLibrary,
                                 depsPackages = depsBuildInfo $ libBuildInfo lib
                               }

depsExecutable :: Executable -> BuildDepends
depsExecutable exe = BuildDepends { depsTarget = TargetExecutable $ exeName exe,
                                    depsPackages = depsBuildInfo $ buildInfo exe
                                  }

depsTestSuite :: TestSuite -> BuildDepends
depsTestSuite ts = BuildDepends { depsTarget = TargetTestSuite $ testName ts,
                                  depsPackages = depsBuildInfo $ testBuildInfo ts
                                }

depsBenchmark :: Benchmark -> BuildDepends
depsBenchmark ben = BuildDepends { depsTarget = TargetBenchmark $ benchmarkName ben,
                                   depsPackages = depsBuildInfo $ benchmarkBuildInfo ben
                                 }
