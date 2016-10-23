-- |
-- Module: Staversion.Internal.BuildPlan.Stackage
-- Description: dealing with Stackage and build-plan repositories online.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan.Stackage
       ( ExactResolver(..),
         PartialResolver(..),
         parseResolverString,
         disambiguate,
         fetchBuildPlanYAML
       ) where

import qualified Data.Text.Lazy as TL

import Staversion.Internal.Query (Resolver)

-- | Non-ambiguous fully-resolved resolver for stackage.
data ExactResolver = ExactLTS Int Int  -- ^ lts-(major).(minor)
                   | ExactNightly Int Int Int -- ^ nightly-(year)-(month)-(day)
                   deriving (Show,Eq,Ord)

-- | Potentially partial resolver for stackage.
data PartialResolver = PartialExact ExactResolver
                     | PartialLTSLatest -- ^ lts (latest)
                     | PartialLTSMajor Int -- ^ lts-(major)
                     | PartialNightlyLatest -- ^ nightly (latest)
                     deriving (Show,Eq,Ord)

parseResolverString :: Resolver -> Maybe PartialResolver
parseResolverString = undefined

-- | Disambigute a 'PartialResolver' by quering the Internet.
disambiguate :: PartialResolver -> IO ExactResolver
disambiguate = undefined

-- | Fetch build plan YAML data from the Internet.
fetchBuildPlanYAML :: ExactResolver -> IO TL.Text
fetchBuildPlanYAML = undefined
