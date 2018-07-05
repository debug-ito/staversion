-- |
-- Module: Staversion.Internal.Exec
-- Description: executable
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Exec
       ( main,
         processCommand,
         -- * Only for testing
         _processCommandWithCustomBuildPlanManager
       ) where

import Control.Applicative ((<$>))
import Control.Monad (mapM_, when)
import Data.Either (rights)
import Data.Function (on)
import Data.List (groupBy, nub)
import Data.Maybe (isJust)
import Data.Text (unpack, pack)
import qualified Data.Text.Lazy.IO as TLIO

import Staversion.Internal.Aggregate (aggregateResults)
import Staversion.Internal.BuildPlan
  ( BuildPlan, packageVersion, buildPlanSource,
    newBuildPlanManager, loadBuildPlan,
    BuildPlanManager, setStackCommand
  )
import Staversion.Internal.Command
  ( parseCommandArgs,
    Command(..)
  )
import Staversion.Internal.Format (formatAggregatedResults)
import qualified Staversion.Internal.Format as Format
import Staversion.Internal.Log (logDebug, logError, Logger, putLogEntry)
import Staversion.Internal.Query
  ( Query(..), PackageSource(..), PackageName, ErrorMsg
  )
import Staversion.Internal.Result
  ( Result(..), ResultBody, ResultBody'(..), ResultSource(..),
    singletonResult
  )
import Staversion.Internal.Cabal (BuildDepends(..), loadCabalFile)

main :: IO ()
main = do
  comm <- parseCommandArgs
  formatAndShow comm =<< aggregate comm =<< (processCommand comm)
  where
    aggregate comm results = case commAggregator comm of
      Nothing -> return $ map singletonResult results
      Just agg -> do
        logDebug (commLogger comm) ("Results before aggregation: " ++ show results)
        let (aresults, logs) = aggregateResults agg results
        mapM_ (putLogEntry $ commLogger comm) logs
        return aresults
    formatAndShow comm aresults = do
      TLIO.putStr $ formatAggregatedResults (commFormatConfig comm) aresults

data ResolvedQuery = RQueryOne PackageName
                   | RQueryCabal FilePath BuildDepends
                   deriving (Show, Eq, Ord)

processCommand :: Command -> IO [Result]
processCommand = _processCommandWithCustomBuildPlanManager return

makeBuildPlanManager :: Command -> IO BuildPlanManager
makeBuildPlanManager comm = do
  man <- newBuildPlanManager (commBuildPlanDir comm) (commLogger comm) (commAllowNetwork comm)
  return $ setStackCommand (commStackCommand comm) man

_processCommandWithCustomBuildPlanManager :: (BuildPlanManager -> IO BuildPlanManager) -> Command -> IO [Result]
_processCommandWithCustomBuildPlanManager customBPM comm = impl where
  impl = do
    bp_man <- customBPM =<< makeBuildPlanManager comm
    query_pairs <- resolveQueries' logger $ commQueries comm
    fmap concat $ mapM (processQueriesIn bp_man query_pairs) $ commSources comm
  logger = commLogger comm
  processQueriesIn bp_man query_pairs source = do
    let queried_names = nub $ concat $ map (getQueriedPackageNames) $ rights $ map snd $ query_pairs
    logDebug logger ("Retrieve package source " ++ show source)
    e_build_plan <- loadBuildPlan bp_man queried_names source
    logBuildPlanResult e_build_plan
    return $ map (makeResult source e_build_plan) $ query_pairs
  makeResult source e_build_plan (orig_query, e_rquery) = case (e_build_plan, e_rquery) of
    (Left error_msg, _) -> resultForBody $ Left error_msg
    (Right _, Left error_msg) -> resultForBody $ Left error_msg
    (Right build_plan, Right rquery) -> resultForBody $ Right $ searchVersions build_plan rquery
    where
      resultForBody body =
        Result { resultIn = ResultSource { resultSourceQueried = source,
                                           resultSourceReal = realSource e_build_plan
                                         },
                 resultFor = orig_query,
                 resultBody = body
               }
  logBuildPlanResult (Right _) = logDebug logger ("Successfully retrieved build plan.")
  logBuildPlanResult (Left error_msg) = logError logger ("Failed to load build plan: " ++ error_msg)

realSource :: Either e BuildPlan -> Maybe PackageSource
realSource (Left _) = Nothing
realSource (Right bp) = Just $ buildPlanSource bp

resolveQueries' :: Logger -> [Query] -> IO [(Query, Either ErrorMsg ResolvedQuery)]
resolveQueries' logger = fmap concat . mapM resolveToList where
  resolveToList query = do
    eret <- resolveQuery logger query
    case eret of
     Right rqueries -> return $ map (\rq -> (query, Right rq)) rqueries
     Left err -> return $ [(query, Left err)]

resolveQuery :: Logger -> Query -> IO (Either ErrorMsg [ResolvedQuery])
resolveQuery _ (QueryName name) = return $ Right $ [RQueryOne name]
resolveQuery logger (QueryCabalFile file) = do
  logDebug logger ("Load " ++ file ++ " for build-depends fields.")
  e_rquery <- (fmap . fmap) processBuildDependsList $ loadCabalFile file
  reportError e_rquery
  return e_rquery
  where
    processBuildDependsList = map (RQueryCabal file) . filter ((0 <) . length . depsPackages)
    reportError e_rquery = case e_rquery of
      Left err -> logError logger err
      Right _ -> return ()

searchVersions :: BuildPlan -> ResolvedQuery -> ResultBody
searchVersions build_plan (RQueryOne package_name) = SimpleResultBody package_name $ packageVersion build_plan package_name
searchVersions build_plan (RQueryCabal cabal_file build_deps) = CabalResultBody cabal_file target ret_list where
  target = depsTarget build_deps
  ret_list = map (\pname -> (pname, packageVersion build_plan pname)) $ depsPackages build_deps

getQueriedPackageNames :: ResolvedQuery -> [PackageName]
getQueriedPackageNames (RQueryOne n) = [n]
getQueriedPackageNames (RQueryCabal _ bd) = depsPackages bd
