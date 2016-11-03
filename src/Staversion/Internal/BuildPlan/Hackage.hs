-- |
-- Module: Staversion.Internal.BuildPlan.Hackage
-- Description: (virtual) BuildPlan expressing the latest Hackage
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan.Hackage
       ( -- * entry API
         RegisteredVersions,
         fetchPreferredVersions,
         latestVersion,
         -- * low-level API
         parsePreferredVersionsJSON
       ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Version (Version)

import Staversion.Internal.Query (ErrorMsg)
import Staversion.Internal.HTTP (Manager)

data RegisteredVersions = RegisteredVersions { regPreferredVersions :: [Version]
                                               -- ^ Sorted list of preferred versions of the package.
                                               -- The head is the latest.
                                             }
                          deriving (Show,Eq,Ord)


parsePreferredVersionsJSON :: BSL.ByteString -> Either ErrorMsg RegisteredVersions
parsePreferredVersionsJSON = undefined

latestVersion :: RegisteredVersions -> Maybe Version
latestVersion = undefined

fetchPreferredVersions :: Manager -> IO (Either ErrorMsg RegisteredVersions)
fetchPreferredVersions = undefined
