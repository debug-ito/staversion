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

import Control.Applicative ((<$>), empty)
import qualified Control.Exception as Exception
import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (sort, reverse)
import Data.Text (unpack)
import Data.Version (Version)

import Staversion.Internal.BuildPlan.Version (unVersionJSON)
import Staversion.Internal.Query (ErrorMsg, PackageName)
import Staversion.Internal.HTTP (Manager, fetchURL, OurHttpException)

data RegisteredVersions = RegisteredVersions { regPreferredVersions :: [Version]
                                               -- ^ Sorted list of preferred versions of the package.
                                               -- The head is the latest.
                                             }
                          deriving (Show,Eq,Ord)

instance FromJSON RegisteredVersions where
  parseJSON (Object o) = (RegisteredVersions . reverse . sort . map unVersionJSON) <$> (o .: "normal-version")
  parseJSON _ = empty

parsePreferredVersionsJSON :: BSL.ByteString -> Either ErrorMsg RegisteredVersions
parsePreferredVersionsJSON = either (\e -> Left ("Decoding preferred versions error: " ++ e)) Right . eitherDecode

latestVersion :: RegisteredVersions -> Maybe Version
latestVersion rvers = case regPreferredVersions rvers of
  [] -> Nothing
  (v : _) -> Just v

fetchPreferredVersions :: Manager -> PackageName -> IO (Either ErrorMsg RegisteredVersions)
fetchPreferredVersions man text_name = Exception.handle handler $ parsePreferredVersionsJSON <$> fetchURL man url where
  name = unpack text_name
  url = "http://hackage.haskell.org/package/" ++ name ++ "/preferred.json"
  handler :: OurHttpException -> IO (Either ErrorMsg a)
  handler e = return $ Left ("HTTP error while fetching versions of " ++ name ++ " in hackage: " ++ show e)
