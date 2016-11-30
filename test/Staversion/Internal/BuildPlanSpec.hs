module Staversion.Internal.BuildPlanSpec (main,spec) where

import Data.Text (Text, pack)
import Data.Version (Version(..))
import Data.Word (Word)
import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.Log (defaultLogger, loggerThreshold)
import Staversion.Internal.Query (PackageName, PackageSource(..))
import Staversion.Internal.BuildPlan
  ( BuildPlanMap, 
    loadBuildPlanMapYAML, 
    packageVersion,
    BuildPlanManager,
    newBuildPlanManager,
    _setLTSDisambiguator,
    loadBuildPlan,
    buildPlanSource
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  packageVersion_spec
  loadBuildPlan_spec

packageVersion_spec :: Spec
packageVersion_spec = describe "packageVersion" $ do
  forBuildPlanMap "conpact_build_plan" $ \loader -> do
    specify "drawille -> 0.1.0.6" $ do
      loadVersion "drawille" loader `shouldReturn` Just (Version [0,1,0,6] [])
    specify "unknown -> Nothing" $ do
      loadVersion "unknown" loader `shouldReturn` Nothing
    specify "ghc -> 7.10.3" $ do
      loadVersion "ghc" loader `shouldReturn` Just (Version [7,10,3] [])
    specify "base -> 4.8.2.0" $ do
      loadVersion "base" loader `shouldReturn` Just (Version [4,8,2,0] [])

  forBuildPlanMap "lts-4.2" $ \loader -> do
    specify "conduit -> 1.2.6.1" $ do
      loadVersion "conduit" loader `shouldReturn` Just (Version [1,2,6,1] [])
    specify "transformers -> 0.4.2.0" $ do
      loadVersion "transformers" loader `shouldReturn` Just (Version [0,4,2,0] [])


forBuildPlanMap :: String -> (IO BuildPlanMap -> Spec) -> Spec
forBuildPlanMap build_plan_base testWith = describe build_plan_base (testWith loader) where
  loader = either error return =<< loadBuildPlanMapYAML ("test" </> "data" </> build_plan_base <.> "yaml")

loadVersion :: PackageName -> IO BuildPlanMap -> IO (Maybe Version)
loadVersion package_name loader = do
  plan <- loader
  return $ packageVersion plan package_name


mockBuildPlanManager :: Word -> Word -> IO BuildPlanManager
mockBuildPlanManager lts_major lts_minor = do
  bp_man <- newBuildPlanManager build_plan_dir logger False
  _setLTSDisambiguator bp_man lts_major lts_minor
  return bp_man
  where
    build_plan_dir = "test" </> "data"
    logger = defaultLogger { loggerThreshold = Nothing }

loadBuildPlan_spec :: Spec
loadBuildPlan_spec = describe "loadBuildPlan" $ do
  it "reads local file after disambiguation" $ do
    bp_man <- mockBuildPlanManager 4 2
    bp <- either (\e -> error ("Error: " ++ e)) return =<< loadBuildPlan bp_man [] (SourceStackage "lts")
    packageVersion bp "base" `shouldBe` (Just $ Version [4,8,2,0] [])
    buildPlanSource bp `shouldBe` SourceStackage "lts-4.2"
