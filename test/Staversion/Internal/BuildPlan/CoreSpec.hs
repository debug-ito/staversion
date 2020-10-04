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
      specGHCAll
      specGHCPlan (mkCompilerVersion [8,8,2]) "bytestring" (Just [0,10,10,0])
      specGHCPlan (mkCompilerVersion [8,8,2]) "foobar" (Nothing)
      specGHCPlan (mkCompilerVersion [7,8,4]) "ghc" (Just [7,8,4])
      specGHCPlan (mkCompilerVersion [7,8,4]) "haskell2010" (Just [1,1,2,0])
      specGHCPlan (mkCompilerVersion [8,4,1]) "Win32" (Just [2,6,1,0])
      specGHCPlan CVHead "Cabal" (Just [3,3,0,0])
      specGHCPlan CVHead "ghc" (Just [8,11,0,20200324])
      specGHCPlan CVHead "Win32" (Just [2,6,1,0])
      specGHCPlan (mkCompilerVersion [8,4,1]) "rts" (Just [1,0])

specGHCAll :: Spec
specGHCAll = do
  specify "load all GHC versions" $ do
    ep <- loadPkgVersions
    case ep of
      Left e -> expectationFailure e
      Right p -> (map compilerVersion $ HM.keys p) `shouldMatchList` exp_versions
  where
    exp_versions = CVHead : (map mkCompilerVersion $ exp_nvers)
    exp_nvers =
      [ [7,0,1],
        [7,0,2],
        [7,0,3],
        [7,0,4],
        [7,2,1],
        [7,2,2],
        [7,4,1],
        [7,4,2],
        [7,6,1],
        [7,6,2],
        [7,6,3],
        [7,8,1],
        [7,8,2],
        [7,8,3],
        [7,8,4],
        [7,10,1],
        [7,10,2],
        [7,10,3],
        [8,0,1],
        [8,0,2],
        [8,2,1],
        [8,2,2],
        [8,4,1],
        [8,4,2],
        [8,4,3],
        [8,4,4],
        [8,6,1],
        [8,6,2],
        [8,6,3],
        [8,6,4],
        [8,6,5],
        [8,8,1],
        [8,8,2],
        [8,8,3],
        [8,10,1]
      ]


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

loadPkgVersions :: IO (Either String (HM.HashMap Compiler CoreBuildPlanMap))
loadPkgVersions = fmap parseGHCPkgVersions $ BSL.readFile pkg_version_file
  where
    pkg_version_file = "test" </> "data" </> "build_plan_ghc" </> "pkg_versions.txt"
  
loadGHCCore :: CompilerVersion -> IO (Either String CoreBuildPlanMap)
loadGHCCore cv = do
  ret <- (fmap . fmap) (HM.lookup ghc) $ loadPkgVersions
  case ret of
    Left err -> return $ Left ("loadGHCCore: parse error: " ++ err)
    Right Nothing -> return $ Left ("loadGHCCore: cannot find build plan for " ++ show ghc)
    Right (Just cbp) -> return $ Right cbp
  where
    ghc = Compiler ghcName cv


