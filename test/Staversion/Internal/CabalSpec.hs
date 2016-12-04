module Staversion.Internal.CabalSpec (main,spec) where

import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.Cabal (loadCabalFile, BuildDepends(..), Target(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "loadCabalFile" $ do
  it "should load library, executable and test-suite targets" $ do
    (Right got_deps) <- loadCabalFile $ "test" </> "data" </> "doctest.cabal"
    got_deps `shouldBe` [ BuildDepends { depsTarget = TargetLibrary,
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
