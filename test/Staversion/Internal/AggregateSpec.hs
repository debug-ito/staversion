module Staversion.Internal.AggregateSpec (main,spec) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Distribution.Version as V
import Test.Hspec

import Staversion.Internal.Aggregate
  ( showVersionRange,
    aggOr,
    aggregateResults
  )
import Staversion.Internal.Query (Resolver, PackageSource(..), Query(..), PackageName)
import Staversion.Internal.Result
  ( Result(..), ResultSource(..), AggregatedResult(..),
    ResultBody'(..)
  )
import Staversion.Internal.TestUtil (ver, simpleResultBody)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_aggregateResults
  describe "Aggregators" $ do
    spec_or

vor :: V.VersionRange -> V.VersionRange -> V.VersionRange
vor = V.unionVersionRanges

vthis :: [Int] -> V.VersionRange
vthis = V.thisVersion . ver

spec_or :: Spec
spec_or = describe "aggOr" $ do
  specify "single version" $ do
    let input = ver [1,2,3] :| []
        expected = V.thisVersion $ ver [1,2,3]
    aggOr input `shouldBe` expected
    (showVersionRange $ aggOr input) `shouldBe` "==1.2.3"
  specify "three versions" $ do
    let input = ver [1,2] :| [ver [3,4], ver [5,6], ver [7,8]]
        expected =   vor (vthis [1,2])
                   $ vor (vthis [3,4])
                   $ vor (vthis [5,6])
                   $ (vthis [7,8])
    aggOr input `shouldBe` expected
    (showVersionRange $ aggOr input) `shouldBe` "==1.2 || ==3.4 || ==5.6 || ==7.8"
  it "should sort versions" $ do
    let input = ver [5,5,0] :| [ver [0,2], ver [5,5], ver [3,3,2,1], ver [0,3]]
        expected =   vor (vthis [0,2])
                   $ vor (vthis [0,3])
                   $ vor (vthis [3,3,2,1])
                   $ vor (vthis [5,5])
                   $ (vthis [5,5,0])
    aggOr input `shouldBe` expected
    (showVersionRange $ aggOr input) `shouldBe` "==0.2 || ==0.3 || ==3.3.2.1 || ==5.5 || ==5.5.0"
  it "should eliminate duplicates" $ do
    let input = ver [1,0] :| [ver [0,4], ver [1,2], ver [0,4], ver [1,0]]
        expected =   vor (vthis [0,4])
                   $ vor (vthis [1,0])
                   $ (vthis [1,2])
    aggOr input `shouldBe` expected

vors :: [[Int]] -> V.VersionRange
vors [] = error "this should not happen"
vors [v] = vthis v
vors (v:rest) = vor (vthis v) $ vors rest

spec_aggregateResults :: Spec
spec_aggregateResults = describe "aggregateResults" $ do
  it "should aggregate SimpleResultBody" $ do
    let input = [ simpleResult "lts-5.0" "hoge" [1,0],
                  simpleResult "lts-6.0" "hoge" [2,0],
                  simpleResult "lts-7.0" "hoge" [3,0]
                ]
        expected = AggregatedResult { aggResultIn = rsource "lts-5.0"
                                                    :| [ rsource "lts-6.0",
                                                         rsource "lts-7.0"
                                                       ],
                                      aggResultFor = QueryName "hoge",
                                      aggResultBody = Right $ SimpleResultBody "hoge"
                                                      $ Just $ vors [[1,0], [2,0], [3,0]]
                                    }
    aggregateResults aggOr input `shouldBe` ([expected], [])

rsource :: Resolver -> ResultSource
rsource res = ResultSource { resultSourceQueried = psource,
                             resultSourceReal = Just psource
                           }
  where
    psource = SourceStackage res

simpleResult :: Resolver -> PackageName -> [Int] -> Result
simpleResult resolver pname version =
  Result { resultIn = rsource resolver,
           resultFor = QueryName pname,
           resultBody = Right $ simpleResultBody pname version
         }
