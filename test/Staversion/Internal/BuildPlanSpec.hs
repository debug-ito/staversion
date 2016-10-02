module Staversion.Internal.BuildPlanSpec (main,spec) where

import Distribution.Package (PackageName(..))
import Distribution.Version (Version(..))
import Test.Hspec

import Staversion.Internal.BuildPlan (BuildPlan, loadBuildPlanYAML, packageVersion)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "packageVersion" $ do
  describe "lts-4.2" $ do
    specify "conduit -> 1.2.6.1" $ do
      plan <- loadBuildPlanYAML "test/data/lts-4.2.yaml"
      packageVersion plan (PackageName "conduit") `shouldBe` Just (Version [1,2,6,1] [])

