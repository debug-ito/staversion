-- |
-- Module: Staversion.Internal.Version
-- Description: Compatibility wrapper for Distribution.Version
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Version
       ( -- * Re-exports
         V.Version,
         V.VersionRange,
         V.LowerBound(..),
         V.UpperBound(..),
         V.Bound(..),
         V.VersionInterval,
         V.thisVersion,
         V.unionVersionRanges,
         V.simplifyVersionRange,
         V.fromVersionIntervals,
         V.mkVersionIntervals,
         V.asVersionIntervals,
         -- * Compatibility
         mkVersion,
         versionNumbers,
         showVersion,
         -- * Util
         parseVersionText
       ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)
import qualified Distribution.Version as V
import Data.Version (parseVersion)
import qualified Data.Version as BaseV
import Text.ParserCombinators.ReadP (readP_to_S)

mkVersion :: [Int] -> V.Version
mkVersion = V.mkVersion

versionNumbers :: V.Version -> [Int]
versionNumbers = V.versionNumbers

showVersion :: V.Version -> String
showVersion = V.showVersion

baseVToV :: BaseV.Version -> V.Version
baseVToV = mkVersion . BaseV.versionBranch

-- | Parse a version text. There must not be any trailing characters
-- after a valid version text.
parseVersionText :: Text -> Maybe V.Version
parseVersionText = extractResult . (readP_to_S parseVersion) . unpack where
  extractResult = fmap baseVToV . listToMaybe . map fst . filter (\pair -> snd pair == "")

