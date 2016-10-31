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
         loadBuildPlanYAMLForResolver,
         ExactResolver(..),
         PartialResolver(..),
         parseResolverString,
         Disambiguator,
         -- * Low level API
         formatResolverString,
         fetchDisambiguator,
         parseDisambiguator,
         fetchBuildPlanYAML
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
import Network.HTTP.Client
  ( parseRequest, Manager,
    httpLbs, responseBody,
    HttpException
  )
import System.IO.Error (ioError, userError)
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf (printf)
import Text.Read.Lex (readDecP)

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

type Disambiguator = PartialResolver -> Maybe ExactResolver

-- | Fetch the 'Disambiguator' from the Internet.
fetchDisambiguator :: Manager -> IO Disambiguator
fetchDisambiguator man = (toExcep . parseDisambiguator) =<< fetchURL man disambiguator_url where
  disambiguator_url = "https://www.stackage.org/download/snapshots.json"
  toExcep (Just d) = return d
  toExcep Nothing = ioError $ userError ("Failed to parse disambiguator from" ++ disambiguator_url)


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

-- | Fetch build plan YAML data from the Internet.
fetchBuildPlanYAML :: Manager -> ExactResolver -> IO BSL.ByteString
fetchBuildPlanYAML man resolver = fetchURL man url where
  resolver_str = formatResolverString $ PartialExact $ resolver
  url = case resolver of
    ExactLTS _ _ -> "https://raw.githubusercontent.com/fpco/lts-haskell/master/" ++ resolver_str ++ ".yaml"
    ExactNightly _ _ _ -> "https://raw.githubusercontent.com/fpco/stackage-nightly/master/" ++ resolver_str ++ ".yaml"

fetchURL :: Manager -> String -> IO BSL.ByteString
fetchURL man url = do
  req <- parseRequest url
  responseBody <$> httpLbs req man

loadBuildPlanYAMLForResolver :: Manager
                             -> Maybe Disambiguator -- ^ the caller may pass a 'Disambiguator'.
                             -> PartialResolver
                             -> IO ((Either ErrorMsg BSL.ByteString), Maybe Disambiguator)
                             -- ^ In sucess, it returns YAML
                             -- 'BSL.ByteString'. It may also return a
                             -- 'Disambiguator' it loaded.
loadBuildPlanYAMLForResolver man m_disam presolver = handleNetworkException m_disam impl where
  handleNetworkException ret_disam = Exception.handle theHandler
    where
      theHandler :: HttpException -> IO ((Either ErrorMsg a), Maybe Disambiguator)
      theHandler e = return $ (Left ("Network error: " ++ show e), ret_disam)
  impl = case presolver of
    PartialExact exact -> processExact exact m_disam
    _ -> do
      (m_exact, got_disam) <- tryDisambiguate
      handleNetworkException (Just got_disam) $ case m_exact of
        Nothing -> return $ (Left ("Could not disambiguate: " ++ show presolver), Just got_disam)
        Just exact -> processExact exact (Just got_disam)
  tryDisambiguate = do
    got_disam <- case m_disam of
      Just d -> return d
      Nothing -> fetchDisambiguator man
    return $ (got_disam presolver, got_disam)
  processExact exact ret_disam = (,) <$> (Right <$> fetchBuildPlanYAML man exact) <*> pure ret_disam
    
  
