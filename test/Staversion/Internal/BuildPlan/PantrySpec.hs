module Staversion.Internal.BuildPlan.PantrySpec (main,spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Text (unpack)
import System.FilePath ((</>), (<.>))
import Test.Hspec

import Staversion.Internal.BuildPlan.BuildPlanMap
  (HasVersions(..), BuildPlanMap)
import qualified Staversion.Internal.BuildPlan.BuildPlanMap as BuildPlanMap
import Staversion.Internal.BuildPlan.Core
  ( Compiler(..),
    mkCompilerVersion,
    CoreBuildPlanMap,
    parseGHCPkgVersions
  )
import Staversion.Internal.BuildPlan.Pantry
  ( PantryBuildPlanMap,
    parseBuildPlanMapYAML,
    pantryCompiler, pantryName,
    coresToBuildPlanMap
  )
import qualified Staversion.Internal.BuildPlan.V1 as V1
import Staversion.Internal.Query (PackageName)
import Staversion.Internal.Version (mkVersion)

import Staversion.Internal.TestUtil (specPackage, stablePList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseBuildPlanMapYAML" $ do
    before (loadBuildPlan ("lts" </> "4" </> "2.yaml")) $ describe "PantryBP lts-4.2" $ do
      specify "pantryCompiler" $ \pbp -> do
        pantryCompiler pbp `shouldBe` (Compiler "ghc" $ mkCompilerVersion [7,10,3])
      specify "pantryName" $ \pbp -> do
        pantryName pbp `shouldBe` Just "lts-4.2"
      specPackage "wrap" (Just [0,0,0])
      specPackage "x509" (Just [1,6,3])
      specPackage "type-level-numbers" (Just [0,1,1,1])
      specPackage "fooooobar" (Nothing)
      specPackage "base" (Nothing)
      specPackage "ghc" (Nothing)
    before (loadBuildPlan ("lts" </> "15" </> "3.yaml")) $ describe "PantryBP lts-15.3" $ do
      specify "pantryCompiler" $ \pbp -> do
        pantryCompiler pbp `shouldBe` (Compiler "ghc" $ mkCompilerVersion [8,8,2])
      specify "pantryName" $ \pbp -> do
        pantryName pbp `shouldBe` Nothing
      specPackage "x509" (Just [1,7,5])
      specPackage "type-level-numbers" (Just [0,1,1,1])
      specPackage "base" (Nothing)
      specPackage "ghc" (Nothing)
    before (loadCompleteBuildPlan ("lts" </> "4" </> "2.yaml")) $ describe "PantryBP lts-4.2 + Core" $ do
      specPackage "wrap" (Just [0,0,0])
      specPackage "x509" (Just [1,6,3])
      specPackage "type-level-numbers" (Just [0,1,1,1])
      specPackage "fooooobar" (Nothing)
      specPackage "base" (Just [4,8,2,0])
      specPackage "ghc" (Just [7,10,3])
      specV1Compatibility "lts-4.2"
      
loadBuildPlan :: FilePath -> IO PantryBuildPlanMap
loadBuildPlan file_subpath = parseIO =<< BS.readFile filepath
  where
    filepath = "test" </> "data" </> "build_plan_pantry" </> file_subpath
    parseIO content = either fail return $ parseBuildPlanMapYAML content

loadCompleteBuildPlan :: FilePath -> IO BuildPlanMap
loadCompleteBuildPlan filename = do
  pbp <- loadBuildPlan filename
  cores <- loadCores
  either fail return $ coresToBuildPlanMap cores pbp
  where
    pkg_versions = "test" </> "data" </> "build_plan_ghc" </> "pkg_versions.txt"
    loadCores = parseCoresIO =<< BSL.readFile pkg_versions
    parseCoresIO = either fail return . parseGHCPkgVersions

specV1Compatibility :: FilePath -> SpecWith BuildPlanMap
specV1Compatibility exp_v1_resolver = do
  specify spec_name $ \pantry_bp -> do
    v1_bp <- either fail return =<< V1.loadBuildPlanMapYAML ("test" </> "data" </> "build_plan_v1" </> exp_v1_resolver <.> "yaml")
    stablePList pantry_bp `shouldMatchList` stablePList v1_bp
  where
    spec_name = exp_v1_resolver ++ ": V1 and Pantry should return the same BuildPlanMap"
