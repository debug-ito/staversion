-- |
-- Module: Staversion.Internal.BuildPlan.V1
-- Description: The legacy "version 1" of build plan YAML files
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- @since 0.2.4.0
module Staversion.Internal.BuildPlan.V1
  ( fetchBuildPlanYAML,
    parseBuildPlanMapYAML,
    loadBuildPlanMapYAML
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), (.:), Value(..), Object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Yaml as Yaml

import Staversion.Internal.HTTP (Manager, fetchURL, OurHttpException)
import Staversion.Internal.Query (PackageName, ErrorMsg)
import Staversion.Internal.BuildPlan.BuildPlanMap (BuildPlanMap)
import qualified Staversion.Internal.BuildPlan.BuildPlanMap as BPMap
import Staversion.Internal.BuildPlan.Stackage (ExactResolver(..), PartialResolver(..), formatResolverString)
import Staversion.Internal.BuildPlan.Version (unVersionJSON)
import Staversion.Internal.Version (Version)


-- | Fetch build plan YAML data from the Internet. This function
-- fetches a build plan YAML file of "version 1" format.
fetchBuildPlanYAML :: Manager -> ExactResolver -> IO BSL.ByteString
fetchBuildPlanYAML man resolver = fetchURL man url where
  resolver_str = formatResolverString $ PartialExact $ resolver
  url = case resolver of
    ExactLTS _ _ -> "https://raw.githubusercontent.com/fpco/lts-haskell/master/" ++ resolver_str ++ ".yaml"
    ExactNightly _ _ _ -> "https://raw.githubusercontent.com/fpco/stackage-nightly/master/" ++ resolver_str ++ ".yaml"

newtype V1BuildPlanMap = V1BuildPlanMap (HM.HashMap PackageName Version) deriving (Show,Eq)

instance FromJSON V1BuildPlanMap where
  parseJSON (Object object) = (\p1 p2 -> V1BuildPlanMap $ p1 <> p2) <$> core_packages <*> other_packages where
    core_packages = parseSysInfo =<< (object .: "system-info")
    parseSysInfo (Object o) = parseCorePackages =<< (o .: "core-packages")
    parseSysInfo _ = empty
    parseCorePackages (Object o) = traverse (\v -> unVersionJSON <$> parseJSON v) o
    parseCorePackages _ = empty

    other_packages = parsePackages =<< (object .: "packages")
    parsePackages (Object o) = traverse parsePackageObject o
    parsePackages _ = empty
    parsePackageObject (Object o) = unVersionJSON <$> (o .: "version")
    parsePackageObject _ = empty
  parseJSON _ = empty

toBuildPlanMap :: V1BuildPlanMap -> BuildPlanMap
toBuildPlanMap (V1BuildPlanMap m) = BPMap.fromMap m

-- | Parse "version 1" format of build plan YAML file.
parseBuildPlanMapYAML :: BS.ByteString -> Either ErrorMsg BuildPlanMap
parseBuildPlanMapYAML = either (Left . toErrorMsg) (Right . toBuildPlanMap)  . Yaml.decodeEither' where
  toErrorMsg parse_exception = "Error while parsing BuildPlanMap YAML: " ++ show parse_exception

-- | Load a 'BuildPlanMap' from a file.
loadBuildPlanMapYAML :: FilePath -> IO (Either ErrorMsg BuildPlanMap)
loadBuildPlanMapYAML yaml_file = parseBuildPlanMapYAML <$> BS.readFile yaml_file where -- TODO: make it memory-efficient!
