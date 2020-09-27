-- |
-- Module: Staversion.Internal.BuildPlan.Stackage
-- Description: dealing with Stackage and build-plan repositories online.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- This module is meant to be exposed only to
-- "Staversion.Internal.BuildPlan" and test modules.
module Staversion.Internal.BuildPlan.Stackage
       ( -- * High level API
         ExactResolver(..),
         PartialResolver(..),
         parseResolverString,
         formatResolverString,
         formatExactResolverString,
         Disambiguator,
         fetchDisambiguator,
         -- * Low level API
         parseDisambiguator
       ) where

import Control.Monad (void)
import Control.Applicative ((<|>), (*>), (<$>), (<*>), empty, pure)
import qualified Control.Exception as Exception (handle)
import Data.Aeson (FromJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Text (unpack)
import Data.Word (Word)
import Data.IORef (IORef)
import System.IO.Error (ioError, userError)
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf (printf)
import Text.Read.Lex (readDecP)

import Staversion.Internal.HTTP (Manager, fetchURL, OurHttpException)
import Staversion.Internal.Query (Resolver, ErrorMsg)

-- | Non-ambiguous fully-resolved resolver for stackage.
data ExactResolver = ExactLTS Word Word  -- ^ lts-(major).(minor)
                   | ExactNightly Word Word Word -- ^ nightly-(year)-(month)-(day)
                   deriving (Show,Eq,Ord)

-- | Potentially partial resolver for stackage.
data PartialResolver = PartialExact ExactResolver
                     | PartialLTSLatest -- ^ lts (latest)
                     | PartialLTSMajor Word -- ^ lts-(major)
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

formatExactResolverString :: ExactResolver -> Resolver
formatExactResolverString er = formatResolverString $ PartialExact er

type Disambiguator = PartialResolver -> Maybe ExactResolver

-- | Fetch the 'Disambiguator' from the Internet.
fetchDisambiguator :: Manager -> IO (Either ErrorMsg Disambiguator)
fetchDisambiguator man = (return . toEither . parseDisambiguator) =<< fetchURL man disambiguator_url where
  disambiguator_url = "https://www.stackage.org/download/snapshots.json"
  toEither = maybe (Left ("Failed to parse disambiguator from" ++ disambiguator_url)) Right

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
  toDisam _ (PartialExact e) = Just e
  toDisam dis_map key = M.lookup key (unDisamMap dis_map)
