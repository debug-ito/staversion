module Staversion.Internal.BuildPlanSpec (main,spec) where

import Data.Text (Text, pack)
import Data.Version (Version(..))
import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.BuildPlan
  ( PackageName,
    BuildPlan, 
    loadBuildPlanYAML, 
    packageVersion,
    parseVersionText
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  parseVersionText_spec
  packageVersion_spec

parseVersionText_spec :: Spec
parseVersionText_spec = describe "parseVersionText" $ do
  spec_v "0.1.0.6" $ Just (Version [0,1,0,6] [])
  spec_v "10.11" $ Just (Version [10,11] [])
  where
    spec_v input_v expected = specify input_v $ do
      parseVersionText (pack input_v) `shouldBe` expected

packageVersion_spec :: Spec
packageVersion_spec = describe "packageVersion" $ do
  forBuildPlan "conpact_build_plan" $ \loader -> do
    specify "drawille -> 0.1.0.6" $ do
      loadVersion "drawille" loader `shouldReturn` Just (Version [0,1,0,6] [])
    specify "unknown -> Nothing" $ do
      loadVersion "unknown" loader `shouldReturn` Nothing
    specify "ghc -> 7.10.3" $ do
      loadVersion "ghc" loader `shouldReturn` Just (Version [7,10,3] [])
    specify "base -> 4.8.2.0" $ do
      loadVersion "base" loader `shouldReturn` Just (Version [4,8,2,0] [])

  forBuildPlan "lts-4.2" $ \loader -> do
    specify "conduit -> 1.2.6.1" $ do
      loadVersion "conduit" loader `shouldReturn` Just (Version [1,2,6,1] [])
    specify "transformers -> 0.4.2.0" $ do
      loadVersion "transformers" loader `shouldReturn` Just (Version [0,4,2,0] [])


forBuildPlan :: String -> (IO BuildPlan -> Spec) -> Spec
forBuildPlan build_plan_base testWith = describe build_plan_base (testWith loader) where
  loader = loadBuildPlanYAML ("test" </> "data" </> build_plan_base <.> "yaml")

loadVersion :: PackageName -> IO BuildPlan -> IO (Maybe Version)
loadVersion package_name loader = do
  plan <- loader
  return $ packageVersion plan package_name
