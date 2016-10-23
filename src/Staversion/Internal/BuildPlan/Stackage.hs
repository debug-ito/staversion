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
         disambiguate,
         fetchBuildPlanYAML
       ) where

import Control.Monad (void)
import Control.Applicative ((<|>), (*>))
import Data.Function (on)
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import qualified Data.Text.Lazy as TL
import qualified Text.ParserCombinators.ReadP as P
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
  lts = P.string "lts" *> ( lts_exact <|> lts_major <|> pure PartialLTSLatest )
  lts_exact = do
    void $ P.char '-'
    major <- decimal
    void $ P.char '.'
    minor <- decimal
    return $ PartialExact $ ExactLTS major minor
  lts_major = P.char '-' *> ( PartialLTSMajor <$> decimal )
  nightly = P.string "nightly" *> ( nightly_exact <|> pure PartialNightlyLatest )
  nightly_exact = do
    void $ P.char '-'
    year <- decimal
    void $ P.char '-'
    month <- decimal
    void $ P.char '-'
    day <- decimal
    return $ PartialExact $ ExactNightly year month day

formatResolverString :: PartialResolver -> Resolver
formatResolverString = undefined

-- | Disambigute a 'PartialResolver' by quering the Internet.
disambiguate :: PartialResolver -> IO ExactResolver
disambiguate = undefined

-- | Fetch build plan YAML data from the Internet.
fetchBuildPlanYAML :: ExactResolver -> IO TL.Text
fetchBuildPlanYAML = undefined
