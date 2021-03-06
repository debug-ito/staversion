module Staversion.Internal.BuildPlan.VersionSpec (main,spec) where

import Data.Text (pack)
import Test.Hspec

import Staversion.Internal.Version (Version, mkVersion)
import Staversion.Internal.BuildPlan.Version (parseVersionText)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parseVersionText_spec

parseVersionText_spec :: Spec
parseVersionText_spec = describe "parseVersionText" $ do
  spec_v "0.1.0.6" $ Just (mkVersion [0,1,0,6])
  spec_v "10.11" $ Just (mkVersion [10,11])
  where
    spec_v input_v expected = specify input_v $ do
      parseVersionText (pack input_v) `shouldBe` expected
