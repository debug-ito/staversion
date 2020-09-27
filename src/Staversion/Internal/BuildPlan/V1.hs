-- |
-- Module: Staversion.Internal.BuildPlan.V1
-- Description: The legacy "version 1" of build plan YAML files
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.BuildPlan.V1
  ( fetchBuildPlanYAML
  ) where

import qualified Data.ByteString.Lazy as BSL

import Staversion.Internal.HTTP (Manager, fetchURL, OurHttpException)
import Staversion.Internal.BuildPlan.Stackage (ExactResolver(..), formatResolverString)

-- | Fetch build plan YAML data from the Internet. This function
-- fetches a build plan YAML file of "version 1" format.
fetchBuildPlanYAML :: Manager -> ExactResolver -> IO BSL.ByteString
fetchBuildPlanYAML man resolver = fetchURL man url where
  resolver_str = formatResolverString $ PartialExact $ resolver
  url = case resolver of
    ExactLTS _ _ -> "https://raw.githubusercontent.com/fpco/lts-haskell/master/" ++ resolver_str ++ ".yaml"
    ExactNightly _ _ _ -> "https://raw.githubusercontent.com/fpco/stackage-nightly/master/" ++ resolver_str ++ ".yaml"

