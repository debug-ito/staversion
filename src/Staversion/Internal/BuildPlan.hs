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

import Distribution.Package (PackageName)
import Distribution.Version (Version)

-- | A data structure that keeps a map between package names and their
-- versions.
data BuildPlan

-- | Load a 'BuildPlan' from a file.
loadBuildPlanYAML :: FilePath -> IO BuildPlan
loadBuildPlanYAML = undefined

packageVersion :: BuildPlan -> PackageName -> Maybe Version
packageVersion = undefined
