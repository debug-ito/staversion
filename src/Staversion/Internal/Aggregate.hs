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
         -- * Utility
         groupAllPreservingOrderBy,
         -- * Low-level functions
         aggregatePackageVersions
       ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<|>))
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Text (unpack)
import Data.Version (Version)
import Distribution.Version (VersionRange)
import qualified Distribution.Version as V
import qualified Distribution.Text as DT
import qualified Text.PrettyPrint as Pretty

import Staversion.Internal.Cabal (Target(..))
import Staversion.Internal.Query (PackageName)
import Staversion.Internal.Log (LogEntry(..), LogLevel(..))
import Staversion.Internal.Result (Result(..), AggregatedResult(..), ResultBody'(..), resultSourceDesc)

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
aggregateInSameQuery aggregate results = impl where
  impl = do
    right_bodies <- toNonEmpty =<< (fmap concat $ mapM getLabeledBody $ NL.toList results)
    let agg_source = fmap (\(_,source,_) -> source) right_bodies
    range_bodies <- aggregateBodies aggregate $ fmap (\(label,_,body) -> (label,body)) right_bodies
    return $ fmap (makeAggregatedResult agg_source) range_bodies
  makeLabel r = "Result in " ++ (unpack $ resultSourceDesc $ resultIn r)
                ++ ", for " ++ (show $ resultFor r)
  getLabeledBody r = case resultBody r of
    Right rbody -> return [(makeLabel r, resultIn r, rbody)]
    Left err -> do
      warn ("Error for " ++ makeLabel r ++ ": " ++ err)
      return []
  makeAggregatedResult agg_source range_body =
    AggregatedResult { aggResultIn = agg_source,
                       aggResultFor = resultFor $ NL.head results,
                       aggResultBody = Right range_body
                     }

aggregateBodies :: Aggregator
                -> NonEmpty (String, ResultBody' (Maybe Version))
                -> AggM (NonEmpty (ResultBody' (Maybe VersionRange)))
aggregateBodies aggregate ver_bodies = case NL.head ver_bodies of
  (_, SimpleResultBody _ _) -> doSimple
  (_, CabalResultBody _ _ _) -> doCabal
  where
    doSimple = do
      labeled_pmaps <- traverse (\(label, body) -> (,) label <$> pmapSimple body) ver_bodies
      range_pmap <- aggregatePackageVersionsM aggregate labeled_pmaps
      case range_pmap of
       [(pname, range)] -> return $ SimpleResultBody pname range :| []
       _ -> bailWithError "Fatal: aggregatePackageVersionsM somehow lost SimpleResultBody package pairs."
    doCabal = do
      let labeledCabal (label, body) = (\(fp,t,pmap) -> (labelWithTarget t label,fp,t,pmap)) <$> pmapCabal body
          -- labeledCabal extracts data from CabalResultBody data constructor.
          labelWithTarget target orig_label = orig_label ++ " target = " ++ show target
          getTarget (_,_,t,_) = t
          aggregateInTargetGroup group@((_, fp, t, _) :| _) =
            fmap (CabalResultBody fp t) $ aggregatePackageVersionsM aggregate $ fmap (\(label, _, _, pmap) -> (label, pmap)) group
      labeled_cabal_groups <- (toNonEmpty . groupAllPreservingOrderBy ((==) `on` getTarget)) =<< (mapM labeledCabal $ NL.toList ver_bodies)
      traverse aggregateInTargetGroup labeled_cabal_groups

inconsistentBodiesError :: AggM a
inconsistentBodiesError = bailWithError "different types of results are mixed."

pmapSimple :: ResultBody' a -> AggM [(PackageName, a)]
pmapSimple (SimpleResultBody pname val) = return [(pname, val)]
pmapSimple _ = inconsistentBodiesError

pmapCabal :: ResultBody' a -> AggM (FilePath, Target, [(PackageName, a)])
pmapCabal (CabalResultBody fp t pmap) = return (fp, t, pmap)
pmapCabal _ = inconsistentBodiesError

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
