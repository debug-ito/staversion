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
         aggPvp,
         -- * Utility
         groupAllPreservingOrderBy,
         -- * Low-level functions
         aggregatePackageVersions
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad (mzero, forM_)
import Control.Applicative ((<$>), (<|>))
import Data.Foldable (foldrM, foldr1)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, All(All))
import Data.List (lookup)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.Version (Version, makeVersion)
import Distribution.Version (VersionRange)
import qualified Distribution.Version as V
import qualified Distribution.Text as DT
import qualified Text.PrettyPrint as Pretty

import Staversion.Internal.Cabal (Target(..))
import Staversion.Internal.Query (PackageName, ErrorMsg)
import Staversion.Internal.Log (LogEntry(..), LogLevel(..))
import Staversion.Internal.Result (Result(..), AggregatedResult(..), ResultBody, ResultBody'(..), resultSourceDesc)

-- | Aggregate some 'Version's into a 'VersionRange'.
type Aggregator = NonEmpty Version -> VersionRange

showVersionRange :: VersionRange -> String
showVersionRange = Pretty.render . DT.disp

groupAllPreservingOrderBy :: (a -> a -> Bool)
                             -- ^ The comparator that determines if the two elements are in the same group.
                             -- This comparator must be transitive, like '(==)'.
                          -> [a] -> [NonEmpty a]
groupAllPreservingOrderBy sameGroup = foldr f [] where
  f item acc = update [] acc where
    update heads [] = (item :| []) : heads
    update heads (cur@(cur_head :| cur_rest) : rest) =
      if sameGroup item cur_head
      then ((item :| (cur_head : cur_rest)) : heads) ++ rest 
      else update (heads ++ [cur]) rest


-- | Aggregator of ORed versions.
aggOr :: Aggregator
aggOr = foldr1 V.unionVersionRanges . fmap V.thisVersion . NL.nub . NL.sort

-- | Aggregate versions to the range that the versions cover in a
-- (strict) PVP sense.
aggPvp :: Aggregator
aggPvp = V.simplifyVersionRange . foldr1 V.unionVersionRanges . fmap toRange . NL.nub . NL.sort where
  toRange v = fromJust $ fmap V.fromVersionIntervals $ V.mkVersionIntervals [(V.LowerBound norm_v V.InclusiveBound, V.UpperBound vu V.ExclusiveBound)] where
    norm_v = makeVersion $ normalizeTralingZeroes $ V.versionBranch v
    vu = makeVersion $ case V.versionBranch norm_v of
      [] -> error "versionBranch must not be empty."
      [x] -> [x, 1]  -- because [x] and [x,0] is equivalent
      (x : y : _) -> [x, y + 1]

normalizeTralingZeroes :: [Int] -> [Int]
normalizeTralingZeroes [] = []
normalizeTralingZeroes (head_v : rest) = head_v : (concat $ dropTrailingZeros $ List.group rest) where
  dropTrailingZeros [] = []
  dropTrailingZeros groups = if and $ map (== 0) $ last groups
                             then init groups
                             else groups

-- | Aggregate 'Result's with the given 'Aggregator'. It first groups
-- 'Result's based on its 'resultFor' field, and then each group is
-- aggregated into an 'AggregatedResult'.
--
-- If it fails, it returns an empty list of 'AggregatedResult'. It
-- also returns a list of 'LogEntry's to report warnings and errors.
aggregateResults :: Aggregator -> [Result] -> ([AggregatedResult], [LogEntry])
aggregateResults aggregate = unMonad
                             . fmap concat
                             . mapM aggregateInSameQuery'
                             . groupAllPreservingOrderBy ((==) `on` resultFor)
  where
    aggregateInSameQuery' results = (fmap NL.toList $ aggregateInSameQuery aggregate results)
                                    <|> return []
    unMonad = (\(magg, logs) -> (toList magg, logs)) . runAggM
    toList Nothing = []
    toList (Just list) = list

aggregateInSameQuery :: Aggregator -> NonEmpty Result -> AggM (NonEmpty AggregatedResult)
aggregateInSameQuery aggregate results = (fmap . fmap) nubAggregatedSources $ impl where
  impl = case partitionResults $ NL.toList results of
    ([], []) -> error "there must be at least one Result"
    (lefts@(left_head : left_rest), []) -> do
      warnLefts lefts
      return $ return $ AggregatedResult { aggResultIn = (resultIn . fst) <$> (left_head :| left_rest),
                                           aggResultFor = resultFor $ fst $ left_head,
                                           aggResultBody = Left $ snd $ left_head
                                         }
    (lefts, (right_head : right_rest)) -> do
      warnLefts lefts
      aggregateRights (right_head :| right_rest)
  warnLefts lefts = forM_ lefts $ \(left_ret, left_err) -> do
    warn ("Error for " ++ makeLabel left_ret ++ ": " ++ left_err)
  makeLabel r = "Result in " ++ (unpack $ resultSourceDesc $ resultIn r)
                ++ ", for " ++ (show $ resultFor r)
  aggregateRights rights = do
    checkConsistentBodies $ fmap snd rights
    right_groups <- toNonEmpty $ groupAllPreservingOrderBy (isSameBodyGroup `on` snd) $ NL.toList rights
    traverse aggregateGroup right_groups
  aggregateGroup group = do
    let agg_source = fmap (\(ret, _) -> resultIn ret) group
    range_body <- aggregateGroupedBodies aggregate
                  $ fmap (\(result, body) -> (makeLabel result ++ makeBodyLabel body, body)) $ group
    return $ makeAggregatedResult agg_source range_body
  makeBodyLabel (SimpleResultBody _ _) = ""
  makeBodyLabel (CabalResultBody _ target _) = ", target " ++ show target
  makeAggregatedResult agg_source range_body =
    AggregatedResult { aggResultIn = agg_source,
                       aggResultFor = resultFor $ NL.head results,
                       aggResultBody = Right range_body
                     }

nubAggregatedSources :: AggregatedResult -> AggregatedResult
nubAggregatedSources input = input { aggResultIn = NL.nub $ aggResultIn input }

partitionResults :: [Result] -> ([(Result, ErrorMsg)], [(Result, ResultBody)])
partitionResults = foldr f ([], []) where
  f ret (lefts, rights) = case resultBody ret of
    Left err -> ((ret, err) : lefts, rights)
    Right body -> (lefts, (ret, body) : rights)

checkConsistentBodies :: NonEmpty ResultBody -> AggM ()
checkConsistentBodies bodies = case bodies of
  (SimpleResultBody _ _ :| rest) -> expectTrue $ mconcat $ map (All . isSimple) rest
  (CabalResultBody _ _ _ :| rest) -> expectTrue $ mconcat $ map (All . isCabal) rest
  where
    isSimple (SimpleResultBody _ _) = True
    isSimple _ = False
    isCabal (CabalResultBody _ _ _) = True
    isCabal _ = False
    expectTrue (All True) = return ()
    expectTrue _ = bailWithError "different types of results are mixed."

isSameBodyGroup :: ResultBody' a -> ResultBody' a -> Bool
isSameBodyGroup (SimpleResultBody _ _) (SimpleResultBody _ _) = True
isSameBodyGroup (CabalResultBody fp_a t_a _) (CabalResultBody fp_b t_b _) = (fp_a == fp_b) && (t_a == t_b)
isSameBodyGroup _ _ = False

pmapInBody :: ResultBody' a -> [(PackageName, a)]
pmapInBody (SimpleResultBody pname val) = [(pname, val)]
pmapInBody (CabalResultBody _ _ pmap) = pmap

aggregateGroupedBodies :: Aggregator
                       -> NonEmpty (String, ResultBody' (Maybe Version))
                       -> AggM (ResultBody' (Maybe VersionRange))
aggregateGroupedBodies aggregate ver_bodies =
  makeBody =<< (aggregatePackageVersionsM aggregate $ fmap toPmap $ ver_bodies)
  where
    toPmap (label, body) = (label, pmapInBody body)
    makeBody range_pmap = case NL.head ver_bodies of
      (_, SimpleResultBody _ _) -> case range_pmap of
        [(pname, vrange)] -> return $ SimpleResultBody pname vrange
        _ -> bailWithError "Fatal: aggregateGroupedBodies somehow lost SimpleResultBody package pairs."
      (_, CabalResultBody fp target _) -> return $ CabalResultBody fp target range_pmap

toNonEmpty :: [a] -> AggM (NonEmpty a)
toNonEmpty [] = mzero
toNonEmpty (h:rest) = return $ h :| rest

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
aggregatePackageVersions ag pm = runAggM $ aggregatePackageVersionsM ag pm


aggregatePackageVersionsM :: Aggregator
                          -> NonEmpty (String, [(PackageName, Maybe Version)])
                          -> AggM [(PackageName, Maybe VersionRange)]
aggregatePackageVersionsM aggregate pmaps = do
  ref_plist <- consistentPackageList $ fmap (\(_, pmap) -> map fst pmap) $ pmaps
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
collectJustVersions pmaps pname = fmap toMaybeNonEmpty $ foldrM f [] pmaps where
  f (label, pmap) acc = case lookup pname pmap of
                         Just (Just v) -> return (v : acc)
                         _ -> warn ("missing version for package "
                                    ++ show pname ++ ": " ++ label) >> return acc
  toMaybeNonEmpty [] = Nothing
  toMaybeNonEmpty (h : rest) = Just $ h :| rest
