module Staversion.Internal.BuildPlan.StackageSpec (main,spec) where

import Control.Applicative ((<$>), (<*>), pure)
import Test.Hspec
import Test.QuickCheck (Arbitrary(..), oneof, property, Gen, suchThat)

import Staversion.Internal.BuildPlan.Stackage
  ( parseResolverString, formatResolverString,
    PartialResolver(..), ExactResolver(..)
  )

nonNega :: (Num a, Ord a, Arbitrary a) => Gen a
nonNega = suchThat arbitrary (>= 0) 

instance Arbitrary ExactResolver where
  arbitrary = oneof [ ExactLTS <$> nonNega <*> nonNega,
                      ExactNightly <$> nonNega <*> nonNega <*> nonNega
                    ]

instance Arbitrary PartialResolver where
  arbitrary = oneof [ PartialExact <$> arbitrary,
                      pure PartialLTSLatest,
                      PartialLTSMajor <$> nonNega,
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
  ex "lts-" Nothing
  ex "nightly-2016" Nothing
  ex "nightly-2016-10-" Nothing
  where
    ex input expected = specify input $ parseResolverString input `shouldBe` expected

format_spec :: Spec
format_spec = describe "formatResolverString" $ do
  it "should generate parsable string" $ property $ \presolver ->
    (parseResolverString $ formatResolverString presolver) == Just presolver
  it "should pad nightly month/day with 0" $ do
    formatResolverString (PartialExact $ ExactNightly 2010 1 8) `shouldBe` "nightly-2010-01-08"
