module Staversion.Internal.BuildPlan.StackageSpec (main,spec) where

import Control.Applicative ((<$>), (<*>), pure)
import Test.Hspec
import Test.QuickCheck (Arbitrary(..), oneof, property)

import Staversion.Internal.BuildPlan.Stackage
  ( parseResolverString, formatResolverString,
    PartialResolver(..), ExactResolver(..)
  )

instance Arbitrary ExactResolver where
  arbitrary = oneof [ ExactLTS <$> arbitrary <*> arbitrary,
                      ExactNightly <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary PartialResolver where
  arbitrary = oneof [ PartialExact <$> arbitrary,
                      pure PartialLTSLatest,
                      PartialLTSMajor <$> arbitrary,
                      pure PartialNightlyLatest
                    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  parse_spec
  format_spec

parse_spec :: Spec
parse_spec = describe "parseResolverString" $ do
  ex "lts-7.0" (Just $ PartialExact $ ExactLTS 7 0)
  ex "lts-2.22" (Just $ PartialExact $ ExactLTS 2 22)
  ex "lts-4" (Just $ PartialLTSMajor 4)
  ex "lts" (Just $ PartialLTSLatest)
  ex "nightly-2016-10-21" (Just $ PartialExact $ ExactNightly 2016 10 21)
  ex "nightly" (Just $ PartialNightlyLatest)
  ex "hoge" Nothing
  ex "lts-5." (Just $ PartialLTSMajor 5)
  ex "lts-" (Just $ PartialLTSLatest)
  ex "nightly-2016" Nothing
  where
    ex input expected = specify input $ parseResolverString input `shouldBe` expected

format_spec :: Spec
format_spec = describe "formatResolverString" $ do
  it "should generate parsable string" $ property $ \presolver ->
    (parseResolverString $ formatResolverString presolver) == Just presolver
