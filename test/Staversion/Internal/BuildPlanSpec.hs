module Staversion.Internal.BuildPlanSpec (main,spec) where

import Data.Text (Text)
import Data.Version (Version(..))
import Test.Hspec

import Staversion.Internal.BuildPlan (BuildPlan, loadBuildPlanYAML, packageVersion)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "packageVersion" $ do
  forBuildPlan "conpact_build_plan" $ \loader -> do
    specify "drawille -> 0.1.0.6" $ do
      loadVersion "drawille" loader `shouldReturn` Just (Version [0,1,0,6] [])
    specify "unknown -> Nothing" $ do
      loadVersion "unknown" loader `shouldReturn` Nothing

  forBuildPlan "lts-4.2" $ \loader -> do
    specify "conduit -> 1.2.6.1" $ do
      loadVersion "conduit" loader `shouldReturn` Just (Version [1,2,6,1] [])

    

forBuildPlan :: String -> (IO BuildPlan -> Spec) -> Spec
forBuildPlan build_plan_base testWith = describe build_plan_base (testWith loader) where
  loader = loadBuildPlanYAML ("test/data/" ++ build_plan_base ++ ".yaml")

loadVersion :: Text -> IO BuildPlan -> IO (Maybe Version)
loadVersion package_name loader = do
  plan <- loader
  return $ packageVersion plan package_name
