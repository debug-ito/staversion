-- |
-- Module: Staversion.Internal.Aggregate
-- Description: aggregation of multiple versions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Aggregate
       ( -- * Top-level function
         aggregateResults,
         -- * Aggregators
         Aggregator,
         VersionRange,
         showVersionRange,
         aggOr,
         -- * Low-level functions
         aggregatePackageVersions
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad (mzero)
import Data.Foldable (foldrM)
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Version (Version)
import Distribution.Version (VersionRange)
import qualified Distribution.Version as V
import qualified Distribution.Text as DT
import qualified Text.PrettyPrint as Pretty

import Staversion.Internal.Query (PackageName)
import Staversion.Internal.Log (LogEntry(..), LogLevel(..))
import Staversion.Internal.Result (Result, AggregatedResult)

-- | Aggregate some 'Version's into a 'VersionRange'.
type Aggregator = NonEmpty Version -> VersionRange

showVersionRange :: VersionRange -> String
showVersionRange = Pretty.render . DT.disp

-- | Aggregator of ORed versions.
aggOr :: Aggregator
aggOr vs = foldr f (V.thisVersion $ NL.last svs) $ NL.init svs where
  svs = NL.nub $ NL.sort vs
  f elem_v range = V.unionVersionRanges (V.thisVersion elem_v) range

-- | Aggregate 'Result's with the given 'Aggregator'. It first groups
-- 'Result's based on its 'resultFor' field, and then each group is
-- aggregated into an 'AggregatedResult'.
--
-- If it fails, it returns an empty list of 'AggregatedResult'. It
-- also returns a list of 'LogEntry's to report warnings and errors.
aggregateResults :: Aggregator -> [Result] -> ([AggregatedResult], [LogEntry])
aggregateResults = undefined

-- | Aggregate one or more maps between 'PackageName' and 'Version'.
--
-- The input 'Maybe' 'Version's should all be 'Just'. 'Nothing' version
-- is warned and ignored. If the input versions are all 'Nothing', the
-- result version range is 'Nothing'.
--
-- The 'PackageName' lists in the input must be consistent (i.e. they
-- all must be the same list.) If not, it returns 'Nothing' map and an
-- error is logged.
aggregatePackageVersions :: Aggregator
                         -> NonEmpty (String, [(PackageName, Maybe Version)])
                         -- ^ (@label@, @version map@). @label@ is used for error logs.
                         -> (Maybe [(PackageName, Maybe VersionRange)], [LogEntry])
aggregatePackageVersions aggregate pmaps = runAggM impl where
  impl = do
    ref_plist <- consistentPackageList $ NL.map (\(label, pmap) -> map fst pmap) $ pmaps
    fmap (zip ref_plist) $ (fmap . fmap . fmap) aggregate $ mapM (collectJustVersions pmaps) ref_plist

-- | Aggregateion monad
type AggM = MaybeT (State.State [LogEntry])

runAggM :: AggM a -> (Maybe a, [LogEntry])
runAggM = reverseLogs . flip State.runState [] . runMaybeT where
  reverseLogs (ret, logs) = (ret, reverse logs)

warn :: String -> AggM ()
warn msg = lift $ State.modify (entry :) where
  entry = LogEntry { logLevel = LogWarn,
                     logMessage = msg
                   }

bailWithError :: String -> AggM a
bailWithError err_msg = (lift $ State.modify (entry :)) >> mzero where
  entry = LogEntry { logLevel = LogError,
                     logMessage = err_msg
                   }

consistentPackageList :: NonEmpty [PackageName] -> AggM [PackageName]
consistentPackageList (ref_list :| rest) = mapM_ check rest >> return ref_list where
  check cur_list = if cur_list == ref_list
                   then return ()
                   else bailWithError ( "package lists are inconsistent:"
                                        ++ " reference list: " ++ show ref_list
                                        ++ ", inconsitent list: " ++ show cur_list
                                      )

collectJustVersions :: NonEmpty (String, [(PackageName, Maybe Version)])
                    -> PackageName
                    -> AggM (Maybe (NonEmpty Version))
collectJustVersions pmaps pname = fmap toNonEmpty $ foldrM f [] pmaps where
  f (label, pmap) acc = case lookup pname pmap of
                         Just (Just v) -> return (v : acc)
                         _ -> warn ("missing version: " ++ label) >> return acc
  toNonEmpty [] = Nothing
  toNonEmpty (h : rest) = Just $ h :| rest
