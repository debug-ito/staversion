-- |
-- Module: Staversion.Internal.BuildPlan
-- Description:  Handle build plan YAML files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan
       ( BuildPlan,
         loadBuildPlanYAML,
         packageVersion
       ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON(..), (.:), Value(..), Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)
import Data.Version (Version)
import qualified Data.Yaml as Yaml
import Text.Read (readMaybe)
  
-- | A data structure that keeps a map between package names and their
-- versions.
newtype BuildPlan = BuildPlan (HM.HashMap Text Version)

instance FromJSON BuildPlan where
  parseJSON (Object object) = toBuildPlan =<< (object .: "packages") where
    toBuildPlan (Object o) = BuildPlan <$> traverse parsePackageObject o
    toBuildPlan _ = empty
    parsePackageObject (Object o) = parseVersionText =<< (o .: "version")
    parsePackageObject _ = empty
    parseVersionText = maybe empty return . readMaybe  . unpack
  parseJSON _ = empty


-- | Load a 'BuildPlan' from a file.
loadBuildPlanYAML :: FilePath -> IO BuildPlan
loadBuildPlanYAML yaml_file = (toException . Yaml.decodeEither') =<< BS.readFile yaml_file where -- TODO: make it memory-efficient!
  toException = either (throwIO) return

packageVersion :: BuildPlan -> Text -> Maybe Version
packageVersion (BuildPlan bp_map) name = HM.lookup name bp_map
