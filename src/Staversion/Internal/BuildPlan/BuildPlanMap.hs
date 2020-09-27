{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: Staversion.Internal.BuildPlan.BuildPlanMap
-- Description: BuildPlanMap type and related symbols.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.BuildPlan.BuildPlanMap
  ( BuildPlanMap,
    fromMap,
    fromList,
    HasVersions(..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import qualified Data.HashMap.Strict as HM
import Data.Monoid (Monoid, (<>), mconcat)
import Data.Semigroup (Semigroup)

import Staversion.Internal.Query
 ( PackageName
 )
import Staversion.Internal.Version (Version)

-- | A data structure that keeps a map between package names and their
-- versions.
newtype BuildPlanMap = BuildPlanMap (HM.HashMap PackageName Version) deriving (Semigroup,Monoid)

fromMap :: HM.HashMap PackageName Version -> BuildPlanMap
fromMap = BuildPlanMap

fromList :: [(PackageName, Version)] -> BuildPlanMap
fromList = BuildPlanMap . HM.fromList

-- | Types that have mapping between 'PackageName' and 'Version'.
class HasVersions t where
  packageVersion :: t -> PackageName -> Maybe Version

instance HasVersions BuildPlanMap where
  packageVersion (BuildPlanMap bp_map) name = HM.lookup name bp_map
