module Staversion.Internal.CabalSpec (main,spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.Cabal (loadCabalFile, BuildDepends(..), Target(..))

main :: IO ()
main = hspec spec

shouldBeParsedTo :: FilePath -> [BuildDepends] -> IO ()
shouldBeParsedTo cabal_file expectation = do
  got_deps <- either error return  =<< loadCabalFile ("test" </> "data" </> cabal_file)
  got_deps `shouldBe` expectation

spec :: Spec
spec = describe "loadCabalFile" $ do
  it "should load library, executable and test-suite targets" $ do
    "doctest.cabal_test" `shouldBeParsedTo`
      [ BuildDepends { depsTarget = TargetLibrary,
                       depsPackages = [ "base",
                                        "base-compat",
                                        "ghc",
                                        "syb",
                                        "deepseq",
                                        "directory",
                                        "filepath",
                                        "process",
                                        "ghc-paths",
                                        "transformers"
                                      ]
                     },
        BuildDepends { depsTarget = TargetExecutable "doctest",
                       depsPackages = [ "base",
                                        "doctest"
                                      ]
                     },
        BuildDepends { depsTarget = TargetTestSuite "spec",
                       depsPackages = [ "base",
                                        "ghc",
                                        "syb",
                                        "deepseq",
                                        "directory",
                                        "filepath",
                                        "process",
                                        "ghc-paths",
                                        "transformers",
                                        "base-compat",
                                        "HUnit",
                                        "hspec",
                                        "QuickCheck",
                                        "stringbuilder",
                                        "silently",
                                        "setenv",
                                        "with-location"
                                      ]
                     },
        BuildDepends { depsTarget = TargetTestSuite "doctests",
                       depsPackages = [ "base", "doctest"
                                      ]
                     }
      ]
  it "should merge conditional build-depends" $ do
    "conduit.cabal_test" `shouldBeParsedTo`
      [ BuildDepends { depsTarget = TargetLibrary,
                       depsPackages = [ "base",
                                        "resourcet",
                                        "exceptions",
                                        "lifted-base",
                                        "transformers-base",
                                        "transformers",
                                        "mtl",
                                        "mmorph",
                                        "monad-control",
                                        "void"
                                      ]
                     },
        BuildDepends { depsTarget = TargetTestSuite "test",
                       depsPackages = [ "conduit",
                                        "base",
                                        "hspec",
                                        "QuickCheck",
                                        "transformers",
                                        "mtl",
                                        "resourcet",
                                        "containers",
                                        "exceptions",
                                        "safe",
                                        "void"
                                      ]
                     },
        BuildDepends { depsTarget = TargetBenchmark "optimize-201408",
                       depsPackages = [ "base",
                                        "conduit",
                                        "vector",
                                        "deepseq",
                                        "containers",
                                        "transformers",
                                        "hspec",
                                        "mwc-random",
                                        "criterion",
                                        "kan-extensions"
                                      ]
                     },
        BuildDepends { depsTarget = TargetBenchmark "unfused",
                       depsPackages = [ "base",
                                        "conduit",
                                        "criterion",
                                        "transformers"
                                      ]
                     }
      ]

  it ( "should handle format with curly-braces."
       ++ "For now, open braces should be at line ends (except for trailing white spaces),"
       ++ " and close braces should be in their own lines (except for leading white spaces"
       ++ " and trailing else block)"
     ) $ do
    "braces.cabal_test" `shouldBeParsedTo`
      [ BuildDepends { depsTarget = TargetLibrary,
                       depsPackages = [ "pack-a", "pack-b", "pack-c", "pack-d", "pack-e"
                                      ]
                     },
        BuildDepends { depsTarget = TargetExecutable "braces-else",
                       depsPackages = [ "base", "pack-a", "pack-b", "pack-c", "pack-d"
                                      ]
                     },
        BuildDepends { depsTarget = TargetTestSuite "braces-nest",
                       depsPackages = [ "base", "pack-a", "pack-b", "pack-c"
                                      ]
                     }
      ]
