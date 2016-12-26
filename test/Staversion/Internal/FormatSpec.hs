module Staversion.Internal.FormatSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import Staversion.Internal.Format (formatResultsCabal)
import Staversion.Internal.Query
  ( PackageSource(..), Query(..),
    Resolver, PackageName
  )
import Staversion.Internal.Result (Result(..), ResultBody(..))
import Staversion.Internal.Cabal (Target(..))

import Staversion.Internal.TestUtil (ver, simpleResultBody, verPairs)
       


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "formatResultsCabal" $ do
  it "should return empty text for empty list" $ do
    formatResultsCabal [] `shouldBe` ""
  it "should format a Result in a Cabal way" $ do
    let input = [ Result { resultIn = SourceStackage "lts-6.10", resultReallyIn = Nothing,
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
    let input = [ Result { resultIn = SourceStackage "lts",
                           resultReallyIn = Just $ SourceStackage "lts-7.4",
                           resultFor = QueryName "foobar",
                           resultBody = Right $ simpleResultBody "foobar" [3,4,5]
                         } ]
        expected = ( "------ lts (lts-7.4)\n"
                     <> "foobar ==3.4.5\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show ERROR if resultBody is Left, resultFor is QueryName" $ do
    let input = [ Result { resultIn = SourceStackage "lts-4.2", resultReallyIn = Nothing,
                           resultFor = QueryName "hogehoge",
                           resultBody = Left "some error"
                         } ]
        expected = ( "------ lts-4.2\n"
                     <> "-- hogehoge ERROR\n"
                     <> "\n"
                   )
    formatResultsCabal input `shouldBe` expected
  it "should show ERROR if resultBody is Left, resultFor is QueryCabalFile" $ do
    let input = [ Result { resultIn = SourceStackage "lts-5.3", resultReallyIn = Nothing,
                           resultFor = QueryCabalFile "foobar.cabal",
                           resultBody = Left "some error"
                         },
                  Result { resultIn = SourceStackage "lts-5.3", resultReallyIn = Nothing,
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
    let mkRet t vps = Result { resultIn = SourceStackage "lts-7.0", resultReallyIn = Nothing,
                               resultFor = QueryCabalFile "hoge.cabal",
                               resultBody = Right $ CabalResultBody  "hoge.cabal" t $ verPairs vps
                             }
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

simpleResult :: Resolver -> PackageName -> [Int] -> Result
simpleResult res name vs = Result { resultIn = SourceStackage res, resultReallyIn = Nothing,
                                    resultFor = QueryName name,
                                    resultBody = Right $ simpleResultBody name vs
                                  }
