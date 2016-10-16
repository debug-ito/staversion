module Staversion.Internal.FormatSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import Staversion.Internal.Format (formatResultsCabal)
import Staversion.Internal.Query
  ( Result(..), PackageSource(..), Query(..),
    Resolver, PackageName
  )

import Staversion.Internal.TestUtil (ver, rvers)
       


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "formatResultsCabal" $ do
  it "should return empty text for empty list" $ do
    formatResultsCabal [] `shouldBe` ""
  it "should format a Result in a Cabal way" $ do
    let input = [ Result { resultIn = SourceStackage "lts-6.10",
                           resultFor = QueryName "hoge",
                           resultVersions = Right $ rvers [("hoge", Just $ ver [3,4,5])]
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

simpleResult :: Resolver -> PackageName -> [Int] -> Result
simpleResult res name vs = Result { resultIn = SourceStackage res,
                                    resultFor = QueryName name,
                                    resultVersions = Right $ rvers [(name, mversion)]
                                  }
  where
    mversion = case vs of
      [] -> Nothing
      _ -> Just $ ver vs
