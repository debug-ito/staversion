module Staversion.Internal.FormatSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import Staversion.Internal.Aggregate (aggOr)
import Staversion.Internal.Format
  ( formatResultsCabal,
    formatResultsCabalAggregated
  )
import Staversion.Internal.Query
  ( PackageSource(..), Query(..),
    Resolver, PackageName
  )
import Staversion.Internal.Result (Result(..), ResultBody(..), ResultSource(..))
import Staversion.Internal.Cabal (Target(..))

import Staversion.Internal.TestUtil (ver, simpleResultBody, verPairs)
       


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_simple
  spec_aggregate

spec_simple :: Spec
spec_simple = describe "formatResultsCabal" $ do
  it "should return empty text for empty list" $ do
    formatResultsCabal [] `shouldBe` ""
  it "should format a Result in a Cabal way" $ do
    let input = [ Result { resultIn = ResultSource (SourceStackage "lts-6.10") Nothing,
                           resultFor = QueryName "hoge",
                           resultBody = Right $ simpleResultBody "hoge" [3,4,5]
                         }
                ]
        expected = ( "------ lts-6.10\n"
                     <> "hoge ==3.4.5\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  
  it "should group Results by PackageSource with preserved order" $ do
    let input = [ simpleResult "source_c" "hoge" [0,5,7],
                  simpleResult "source_a" "hoge" [1,0,0],
                  simpleResult "source_b" "foobar" [0,2,5,3],
                  simpleResult "source_b" "hoge" [1,2,0],
                  simpleResult "source_a" "quux" [300,5],
                  simpleResult "source_a" "foobar" [0,3,2],
                  simpleResult "source_b" "quux" [299,10]
                ]
        expected = ( "------ source_c\n"
                     <> "hoge ==0.5.7\n"
                     <> "\n"
                     <> "------ source_a\n"
                     <> "hoge ==1.0.0,\n"
                     <> "quux ==300.5,\n"
                     <> "foobar ==0.3.2\n"
                     <> "\n"
                     <> "------ source_b\n"
                     <> "foobar ==0.2.5.3,\n"
                     <> "hoge ==1.2.0,\n"
                     <> "quux ==299.10\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should group Results by PackageSource with preserved order (2)" $ do
    let input = [ simpleResult "lts-5.0" "pack-A" [1,0],
                  simpleResult "lts-5.0" "pack-A" [2,0],
                  simpleResult "lts-6.6" "pack-A" [3,0],
                  simpleResult "lts-6.6" "pack-A" [4,0],
                  simpleResult "lts-8.0" "pack-A" [5,0],
                  simpleResult "lts-6.6" "pack-A" [6,0],
                  simpleResult "lts-6.6" "pack-A" [7,0]
                ]
        expected = ( "------ lts-5.0\n"
                     <> "pack-A ==1.0,\n"
                     <> "pack-A ==2.0\n"
                     <> "\n"
                     <> "------ lts-6.6\n"
                     <> "pack-A ==3.0,\n"
                     <> "pack-A ==4.0,\n"
                     <> "pack-A ==6.0,\n"
                     <> "pack-A ==7.0\n"
                     <> "\n"
                     <> "------ lts-8.0\n"
                     <> "pack-A ==5.0\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should not put comma at the last non-N/A entry even if it is followed by N/A entries"  $ do
    let input = [ simpleResult "s" "hoge" [1,0,0],
                  simpleResult "s" "not-found-1" [],
                  simpleResult "s" "foobar" [2,0,0],
                  simpleResult "s" "not-found-2" [],
                  simpleResult "s" "not-found-3" []
                ]
        expected = ( "------ s\n"
                     <> "hoge ==1.0.0,\n"
                     <> "-- not-found-1 N/A,\n"
                     <> "foobar ==2.0.0\n"
                     <> "-- not-found-2 N/A,\n"
                     <> "-- not-found-3 N/A\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should output resultReallyIn field" $ do
    let input = [ Result { resultIn = ResultSource (SourceStackage "lts") (Just $ SourceStackage "lts-7.4"),
                           resultFor = QueryName "foobar",
                           resultBody = Right $ simpleResultBody "foobar" [3,4,5]
                         } ]
        expected = ( "------ lts (lts-7.4)\n"
                     <> "foobar ==3.4.5\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show ERROR if resultBody is Left, resultFor is QueryName" $ do
    let input = [ Result { resultIn = ResultSource (SourceStackage "lts-4.2") Nothing,
                           resultFor = QueryName "hogehoge",
                           resultBody = Left "some error"
                         } ]
        expected = ( "------ lts-4.2\n"
                     <> "-- hogehoge ERROR\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show ERROR if resultBody is Left, resultFor is QueryCabalFile" $ do
    let input = [ Result { resultIn = ResultSource (SourceStackage "lts-5.3") Nothing,
                           resultFor = QueryCabalFile "foobar.cabal",
                           resultBody = Left "some error"
                         },
                  Result { resultIn = ResultSource (SourceStackage "lts-5.3") Nothing,
                           resultFor = QueryName "hoge",
                           resultBody = Right $ simpleResultBody "hoge" [5,5]
                         }
                ]
        expected = ( "------ lts-5.3\n"
                     <> "-- foobar.cabal ERROR\n"
                     <> "\n"
                     <> "hoge ==5.5\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show build-depends blocks for CabalResultBody" $ do
    let mkRet = cabalResult "lts-7.0" "hoge.cabal"
        input = [ mkRet TargetLibrary [ ("base", [4,6,0,0]),
                                        ("foobar", [5,7])
                                      ],
                  mkRet (TargetExecutable "hoge-exe") [ ("bytestring", [1,9]) ],
                  mkRet (TargetTestSuite "hoge-test") [ ("hspec", [10,8,9]),
                                                        ("QuickCheck", [5,4,2]),
                                                        ("unknown", [])
                                                      ],
                  mkRet (TargetBenchmark "hoge-bench") [ ("base", [4,8,0,4]),
                                                         ("quux", []),
                                                         ("parsec", [3,0,2])
                                                       ]
                ]
        expected = ( "------ lts-7.0\n"
                     <> "-- hoge.cabal - library\n"
                     <> "base ==4.6.0.0,\n"
                     <> "foobar ==5.7\n"
                     <> "\n"
                     <> "-- hoge.cabal - executable hoge-exe\n"
                     <> "bytestring ==1.9\n"
                     <> "\n"
                     <> "-- hoge.cabal - test-suite hoge-test\n"
                     <> "hspec ==10.8.9,\n"
                     <> "QuickCheck ==5.4.2\n"
                     <> "-- unknown N/A\n"
                     <> "\n"
                     <> "-- hoge.cabal - benchmark hoge-bench\n"
                     <> "base ==4.8.0.4,\n"
                     <> "-- quux N/A,\n"
                     <> "parsec ==3.0.2\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show mixed blocks and lines" $ do
    let input = [ simpleResult "lts-5.0" "pack-A" [4,5],
                  simpleResult "lts-5.0" "pack-B" [7,7],
                  cabalResult "lts-5.0" "X.cabal" TargetLibrary [("pack-A", [4,5,1]), ("pack-B", [6,0])],
                  cabalResult "lts-5.0" "X.cabal" (TargetExecutable "X-exe") [("pack-A", []), ("pack-B", [6,6]), ("pack-C", [0,10])],
                  cabalResult "lts-6.6" "Y.cabal" TargetLibrary [("pack-C", [9,99,0])],
                  simpleResult "lts-6.6" "pack-A" [],
                  simpleResult "lts-7.2" "pack-B" [8,4],
                  cabalResult "lts-6.6" "Y.cabal" (TargetTestSuite "X-test") [("pack-D", [1,0]), ("pack-A", [5,0]), ("pack-C", []), ("pack-B", [6,5])]
                ]
        expected = ( "------ lts-5.0\n"
                     <> "pack-A ==4.5,\n"
                     <> "pack-B ==7.7\n"
                     <> "\n"
                     <> "-- X.cabal - library\n"
                     <> "pack-A ==4.5.1,\n"
                     <> "pack-B ==6.0\n"
                     <> "\n"
                     <> "-- X.cabal - executable X-exe\n"
                     <> "-- pack-A N/A,\n"
                     <> "pack-B ==6.6,\n"
                     <> "pack-C ==0.10\n"
                     <> "\n"
                     <> "------ lts-6.6\n"
                     <> "-- Y.cabal - library\n"
                     <> "pack-C ==9.99.0\n"
                     <> "\n"
                     <> "-- pack-A N/A\n"
                     <> "\n"
                     <> "-- Y.cabal - test-suite X-test\n"
                     <> "pack-D ==1.0,\n"
                     <> "pack-A ==5.0,\n"
                     <> "-- pack-C N/A,\n"
                     <> "pack-B ==6.5\n"
                     <> "\n"
                     <> "------ lts-7.2\n"
                     <> "pack-B ==8.4\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected

spec_aggregate :: Spec
spec_aggregate = describe "formatResultsCabalAggregated" $ do
  it "should aggregate Results over multiple package sources" $ do
    let input = [ simpleResult "lts-4.2" "hoge" [1,2,3],
                  simpleResult "lts-5.0" "hoge" [1,5]
                ]
        expected = ( "------ lts-4.2, lts-5.0\n"
                     <> "hoge ==1.2.3 || ==1.5\n"
                     <> "\n"
                   )
    -- formatResultsCabalAggregated aggOr input `shouldBe` expected
    pending
  it "should show resultReallyIn in the header" $ do
    let input = [ setRealSource "lts-4.22" $ simpleResult "lts-4" "foobar" [2,3],
                  hackageResult "foobar" [2,3,10],
                  simpleResult "lts-5.3" "foobar" [2,3,4]
                ]
        expected = ( "------ lts-4 (lts-4.22), latest in hackage, lts-5.3\n"
                     <> "foobar ==2.3 || ==2.3.4 || ==2.3.10\n"
                     <> "\n"
                   )
    -- formatResultsCabalAggregated aggOr input `shouldBe` expected
    pending

simpleResult :: Resolver -> PackageName -> [Int] -> Result
simpleResult res name vs = Result { resultIn = ResultSource (SourceStackage res) Nothing,
                                    resultFor = QueryName name,
                                    resultBody = Right $ simpleResultBody name vs
                                  }

hackageResult :: PackageName -> [Int] -> Result
hackageResult name vs = Result { resultIn = ResultSource SourceHackage Nothing,
                                 resultFor = QueryName name,
                                 resultBody = Right $ simpleResultBody name vs
                               }

cabalResult :: Resolver -> FilePath -> Target -> [(PackageName, [Int])] -> Result
cabalResult res file target vps =
  Result { resultIn = ResultSource (SourceStackage res) Nothing,
           resultFor = QueryCabalFile file,
           resultBody = Right $ CabalResultBody file target $ verPairs vps
         }

setRealSource :: Resolver -> Result -> Result
setRealSource resolver ret = ret { resultIn = rin { resultSourceReal = Just $ SourceStackage resolver } } where
  rin = resultIn ret
