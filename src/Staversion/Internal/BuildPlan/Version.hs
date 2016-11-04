-- |
-- Module: Staversion.Internal.BuildPlan.Version
-- Description: parsing Version
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan.Version
       ( parseVersionText,
         VersionJSON(..)
       ) where

import Control.Applicative (empty, pure)
import Data.Aeson (FromJSON(..), Value(..))
import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)
import Data.Version (Version, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

-- | Parse a version text. There must not be any trailing characters
-- after a valid version text.
parseVersionText :: Text -> Maybe Version
parseVersionText = extractResult . (readP_to_S parseVersion) . unpack where
  extractResult = listToMaybe . map fst . filter (\pair -> snd pair == "")

-- | a wrapper around 'Version' for JSON I/F
newtype VersionJSON = VersionJSON { unVersionJSON :: Version } deriving (Show,Eq,Ord)

instance FromJSON VersionJSON where
  parseJSON (String t) = maybe empty pure $ fmap VersionJSON $ parseVersionText t
  parseJSON _ = empty
