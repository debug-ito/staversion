-- |
-- Module: Staversion.Internal.BuildPlan.StackYaml
-- Description: Get PackageSource from stack.yaml
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- This module is meant to be exposed only to
-- "Staversion.Internal.BuildPlan" and test modules.
module Staversion.Internal.BuildPlan.StackYaml
       ( readResolver
       ) where

import Staversion.Internal.Query (Resolver, ErrorMsg)

readResolver :: FilePath -- ^ path to stack.yaml
             -> IO (Either ErrorMsg Resolver)
readResolver = undefined -- TODO
