module Staversion.Internal.BuildPlanSpec (main,spec) where

import Data.Text (Text, pack)
import Data.Word (Word)
import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.Log (defaultLogger, loggerThreshold)
import Staversion.Internal.Query (PackageName, PackageSource(..))
import Staversion.Internal.BuildPlan
  ( BuildPlanMap, 
    packageVersion,
    BuildPlanManager,
    newBuildPlanManager,
    _setLTSDisambiguator,
    loadBuildPlan,
    buildPlanSource
  )
import Staversion.Internal.BuildPlan.V1 (loadBuildPlanMapYAML)
import Staversion.Internal.Version (Version)

import Staversion.Internal.TestUtil (ver)


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
      loadVersion "drawille" loader `shouldReturn` Just (ver [0,1,0,6])
    specify "unknown -> Nothing" $ do
      loadVersion "unknown" loader `shouldReturn` Nothing
    specify "ghc -> 7.10.3" $ do
      loadVersion "ghc" loader `shouldReturn` Just (ver [7,10,3])
    specify "base -> 4.8.2.0" $ do
      loadVersion "base" loader `shouldReturn` Just (ver [4,8,2,0])

  forBuildPlanMap "lts-4.2" $ \loader -> do
    specify "conduit -> 1.2.6.1" $ do
      loadVersion "conduit" loader `shouldReturn` Just (ver [1,2,6,1])
    specify "transformers -> 0.4.2.0" $ do
      loadVersion "transformers" loader `shouldReturn` Just (ver [0,4,2,0])


forBuildPlanMap :: String -> (IO BuildPlanMap -> Spec) -> Spec
forBuildPlanMap build_plan_base testWith = describe build_plan_base (testWith loader) where
  loader = either error return =<< loadBuildPlanMapYAML ("test" </> "data" </> "build_plan_v1" </> build_plan_base <.> "yaml")

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
    build_plan_dir = "test" </> "data" </> "build_plan_v1"
    logger = defaultLogger { loggerThreshold = Nothing }

loadBuildPlan_spec :: Spec
loadBuildPlan_spec = describe "loadBuildPlan" $ do
  let expRight = either (\e -> error ("Error: " ++ e)) return
  it "reads local file after disambiguation" $ do
    bp_man <- mockBuildPlanManager 4 2
    bp <- expRight =<< loadBuildPlan bp_man [] (SourceStackage "lts")
    packageVersion bp "base" `shouldBe` (Just $ ver [4,8,2,0])
    buildPlanSource bp `shouldBe` SourceStackage "lts-4.2"
  it "reads the given stack.yaml for resolver" $ do
    bp_man <- mockBuildPlanManager 4 2
    bp <- expRight =<< loadBuildPlan bp_man [] (SourceStackYaml ("test" </> "data" </> "stack" </> "stack_sample.yaml"))
    packageVersion bp "optparse-applicative" `shouldBe` (Just $ ver [0,12,0,0])
    buildPlanSource bp `shouldBe` SourceStackage "lts-4.2"
