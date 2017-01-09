module Staversion.Internal.AggregateSpec (main,spec) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Distribution.Version as V
import Test.Hspec

import Staversion.Internal.TestUtil (ver)
import Staversion.Internal.Aggregate (aggOr)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Aggregators" $ do
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
  specify "three versions" $ do
    let input = ver [1,2] :| [ver [3,4], ver [5,6], ver [7,8]]
        expected =   vor (vthis [1,2])
                   $ vor (vthis [3,4])
                   $ vor (vthis [5,6])
                   $ (vthis [7,8])
    aggOr input `shouldBe` expected
  it "should sort versions" $ do
    let input = ver [5,5,0] :| [ver [0,2], ver [5,5], ver [3,3,2,1], ver [0,3]]
        expected =   vor (vthis [0,2])
                   $ vor (vthis [0,3])
                   $ vor (vthis [3,3,2,1])
                   $ vor (vthis [5,5])
                   $ (vthis [5,5,0])
    aggOr input `shouldBe` expected
