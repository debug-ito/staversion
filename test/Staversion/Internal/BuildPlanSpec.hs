module Staversion.Internal.BuildPlanSpec (main,spec) where

import Data.Text (Text, pack)
import Data.Version (Version(..))
import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.Log (defaultLogger, loggerThreshold)
import Staversion.Internal.Query (PackageName, PackageSource(..))
import Staversion.Internal.BuildPlan
  ( BuildPlan, 
    loadBuildPlanYAML, 
    packageVersion,
    parseVersionText,
    BuildPlanManager,
    newBuildPlanManager,
    _setDisambiguator,
    loadBuildPlan
  )
import Staversion.Internal.BuildPlan.Stackage
  ( Disambiguator,
    PartialResolver(..), ExactResolver(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  parseVersionText_spec
  packageVersion_spec
  loadBuildPlan_spec

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
  loader = either error return =<< loadBuildPlanYAML ("test" </> "data" </> build_plan_base <.> "yaml")

loadVersion :: PackageName -> IO BuildPlan -> IO (Maybe Version)
loadVersion package_name loader = do
  plan <- loader
  return $ packageVersion plan package_name


mockBuildPlanManager :: Disambiguator -> IO BuildPlanManager
mockBuildPlanManager disam = do
  bp_man <- newBuildPlanManager build_plan_dir logger False
  _setDisambiguator bp_man (Just disam)
  return bp_man
  where
    build_plan_dir = "test" </> "data"
    logger = defaultLogger { loggerThreshold = Nothing }

loadBuildPlan_spec :: Spec
loadBuildPlan_spec = describe "loadBuildPlan" $ do
  it "reads local file after disambiguation" $ do
    let disam (PartialLTSLatest) = Just $ ExactLTS 4 2
        disam _ = Nothing
    bp_man <- mockBuildPlanManager disam
    bp <- either (\e -> error ("Error: " ++ e)) return =<< loadBuildPlan bp_man (SourceStackage "lts")
    packageVersion bp "base" `shouldBe` (Just $ Version [4,8,2,0] [])
