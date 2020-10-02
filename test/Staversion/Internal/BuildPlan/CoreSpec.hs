module Staversion.Internal.BuildPlan.CoreSpec (main,spec) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Text (unpack)
import System.FilePath ((</>))
import Test.Hspec

import Staversion.Internal.BuildPlan.BuildPlanMap (HasVersions(..))
import Staversion.Internal.BuildPlan.Core
  ( CoreBuildPlanMap(..),
    CompilerVersion(..),
    Compiler(..),
    parseGHCPkgVersions,
    ghcName,
    mkCompilerVersion
  )
import Staversion.Internal.Query (PackageName)
import Staversion.Internal.Version (Version, mkVersion)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CoreBuildPlanMap" $ do
    describe "ghc" $ do
      specGHCPlan (mkCompilerVersion [8,8,2]) "bytestring" (Just [0,10,10,0])
      specGHCPlan (mkCompilerVersion [8,8,2]) "foobar" (Nothing)
      specGHCPlan (mkCompilerVersion [7,8,4]) "ghc" (Just [7,8,4])
      specGHCPlan (mkCompilerVersion [7,8,4]) "haskell2010" (Just [1,1,2,0])
      specGHCPlan (mkCompilerVersion [8,4,1]) "Win32" (Just [2,6,1,0])
      specGHCPlan CVHead "Cabal" (Just [3,3,0,0])
      specGHCPlan CVHead "ghc" (Just [8,11,0,20200324])
      specGHCPlan CVHead "Win32" (Just [2,6,1,0])

specGHCPlan :: CompilerVersion -- ^ GHC version
            -> PackageName -- ^ Package to query
            -> Maybe [Int] -- ^ Expected version
            -> Spec
specGHCPlan cv pname exp_version_list = do
  specify spec_name $ do
    ecbp <- loadGHCCore cv
    case ecbp of
      Left err -> expectationFailure err
      Right cbp -> do
        coreCompiler cbp `shouldBe` exp_compiler
        packageVersion cbp pname `shouldBe` exp_version
  where
    spec_name = "ghc " ++ show cv ++ ", query " ++ unpack pname
    exp_compiler = Compiler ghcName cv
    exp_version = fmap mkVersion $ exp_version_list

loadGHCCore :: CompilerVersion -> IO (Either String CoreBuildPlanMap)
loadGHCCore cv = do
  ret <- fmap (fmap (HM.lookup ghc) . parseGHCPkgVersions) $ BSL.readFile pkg_version_file
  case ret of
    Left err -> return $ Left ("loadGHCCore: parse error: " ++ err)
    Right Nothing -> return $ Left ("loadGHCCore: cannot find build plan for " ++ show ghc)
    Right (Just cbp) -> return $ Right cbp
  where
    pkg_version_file = "test" </> "data" </> "build_plan_ghc" </> "pkg_versions.txt"
    ghc = Compiler ghcName cv


