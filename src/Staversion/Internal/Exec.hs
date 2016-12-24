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
import Data.Function (on)
import Data.List (groupBy, nub)
import Data.Text (unpack)
import qualified Data.Text.Lazy.IO as TLIO

import Staversion.Internal.BuildPlan
  ( BuildPlan, packageVersion, buildPlanSource,
    newBuildPlanManager, loadBuildPlan,
    BuildPlanManager
  )
import Staversion.Internal.Command
  ( parseCommandArgs,
    Command(..)
  )
import Staversion.Internal.Format (formatResultsCabal)
import Staversion.Internal.Log (logDebug, logError, Logger)
import Staversion.Internal.Query
  ( Query(..), Result(..), PackageSource(..), PackageName,
    ResultBody(..),
    ErrorMsg
  )
import Staversion.Internal.Cabal (BuildDepends(..), loadCabalFile)

main :: IO ()
main = do
  comm <- parseCommandArgs
  (TLIO.putStr . formatResultsCabal) =<< (processCommand comm)

data ResolvedQuery = RQueryOne Query PackageName
                   | RQueryCabal Query FilePath BuildDepends
                   deriving (Show, Eq, Ord)

processCommand :: Command -> IO [Result]
processCommand = _processCommandWithCustomBuildPlanManager return

_processCommandWithCustomBuildPlanManager :: (BuildPlanManager -> IO BuildPlanManager) -> Command -> IO [Result]
_processCommandWithCustomBuildPlanManager customBPM comm = impl where
  impl = do
    bp_man <- customBPM =<< newBuildPlanManager (commBuildPlanDir comm) (commLogger comm) (commAllowNetwork comm)
    rqueries <- resolveQueries' logger $ commQueries comm
    fmap concat $ mapM (processQueriesIn bp_man rqueries) $ commSources comm
  logger = commLogger comm
  processQueriesIn bp_man rqueries source = do
    let queried_names = nub $ concat $ map getQueriedPackageNames $ rqueries
    logDebug logger ("Retrieve package source " ++ show source)
    e_build_plan <- loadBuildPlan bp_man queried_names source
    logBuildPlanResult e_build_plan
    return $ map (makeResult source e_build_plan) $ rqueries
  makeResult source e_build_plan rquery = case e_build_plan of
    Left error_msg -> Result { resultIn = source, resultReallyIn = Nothing,
                               resultFor = originalQuery rquery, resultBody = Left error_msg
                             }
    Right build_plan -> Result { resultIn = source,
                                 resultReallyIn = if source == real_source then Nothing else Just real_source,
                                 resultFor = originalQuery rquery,
                                 resultBody = Right $ searchVersions build_plan rquery
                               }
      where real_source = buildPlanSource build_plan
  logBuildPlanResult (Right _) = logDebug logger ("Successfully retrieved build plan.")
  logBuildPlanResult (Left error_msg) = logError logger ("Failed to load build plan: " ++ error_msg)

resolveQueries' :: Logger -> [Query] -> IO [ResolvedQuery]
resolveQueries' logger = fmap concat . mapM resolveQ where
  resolveQ query = reportAndFilterError =<< resolveQuery query
  reportAndFilterError (Left err) = logError logger err >> return []
  reportAndFilterError (Right ret) = return ret

resolveQuery :: Query -> IO (Either ErrorMsg [ResolvedQuery])
resolveQuery q@(QueryName name) = return $ Right $ [RQueryOne q name]
resolveQuery q@(QueryCabalFile file) = (fmap . fmap) (map (RQueryCabal q file)) $ loadCabalFile file

originalQuery :: ResolvedQuery -> Query
originalQuery (RQueryOne q _) = q
originalQuery (RQueryCabal q _ _) = q

searchVersions :: BuildPlan -> ResolvedQuery -> ResultBody
searchVersions build_plan (RQueryOne _ package_name) = SimpleResultBody package_name $ packageVersion build_plan package_name
searchVersions _ (RQueryCabal _ _ _) = undefined -- TODO

getQueriedPackageNames :: ResolvedQuery -> [PackageName]
getQueriedPackageNames (RQueryOne _ n) = [n]
getQueriedPackageNames (RQueryCabal _ _ bd) = depsPackages bd
