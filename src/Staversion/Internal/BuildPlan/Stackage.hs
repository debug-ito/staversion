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
         formatResolverString,
         Disambiguator,
         fetchDisambiguator,
         parseDisambiguator,
         fetchBuildPlanYAML
       ) where

import Control.Monad (void)
import Control.Applicative ((<|>), (*>), (<$>), (<*>), empty)
import Data.Aeson (FromJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Text (unpack)
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf (printf)
import Text.Read.Lex (readDecP)

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
parseResolverString = getResult . P.readP_to_S parser where
  getResult = fmap fst . listToMaybe . sortBy (compare `on` (length . snd))
  decimal = readDecP
  parser = lts <|> nightly
  lts = P.string "lts" *> ( lts_exact <|> lts_major <|> (P.eof *> pure PartialLTSLatest) )
  lts_exact = do
    void $ P.char '-'
    major <- decimal
    void $ P.char '.'
    minor <- decimal
    return $ PartialExact $ ExactLTS major minor
  lts_major = P.char '-' *> ( PartialLTSMajor <$> decimal )
  nightly = P.string "nightly" *> ( nightly_exact <|> (P.eof *> pure PartialNightlyLatest) )
  nightly_exact = do
    void $ P.char '-'
    year <- decimal
    void $ P.char '-'
    month <- decimal
    void $ P.char '-'
    day <- decimal
    return $ PartialExact $ ExactNightly year month day

formatResolverString :: PartialResolver -> Resolver
formatResolverString pr = case pr of
  PartialExact (ExactLTS major minor) -> "lts-" ++ show major ++ "." ++ show minor
  PartialExact (ExactNightly year month day) -> printf "nightly-%04d-%02d-%02d" year month day
  PartialLTSLatest -> "lts"
  PartialLTSMajor major -> "lts-" ++ show major
  PartialNightlyLatest -> "nightly"

type Disambiguator = PartialResolver -> Maybe ExactResolver

-- | Fetch the 'Disambiguator' from the Internet.
fetchDisambiguator :: IO Disambiguator
fetchDisambiguator = undefined


newtype DisamMap = DisamMap { unDisamMap :: M.Map PartialResolver ExactResolver }

instance FromJSON DisamMap where
  parseJSON (Object o) = fmap (DisamMap . M.fromList) $ mapM parsePair $ HM.toList o where
    parsePair (k,v) = (,) <$> parseKey k <*> parseValue v
    parseKey key = maybe empty return $ parseResolverString $ unpack key
    parseValue v = (expectExact . parseResolverString) =<< parseJSON v
    expectExact (Just (PartialExact e)) = return e
    expectExact _ = empty
  parseJSON _ = empty

parseDisambiguator :: BSL.ByteString -- ^ disambiguation JSON text.
                   -> Maybe Disambiguator
parseDisambiguator input = toDisam <$> Aeson.decode input where
  toDisam dis_map key = M.lookup key (unDisamMap dis_map)

-- | Fetch build plan YAML data from the Internet.
fetchBuildPlanYAML :: ExactResolver -> IO BSL.ByteString
fetchBuildPlanYAML = undefined
