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
import Staversion.Internal.Log (logDebug, logError)
import Staversion.Internal.Query
  ( Query(..), Result(..), PackageSource(..), PackageName,
    ResultBody(..),
    ErrorMsg
  )

main :: IO ()
main = do
  comm <- parseCommandArgs
  (TLIO.putStr . formatResultsCabal) =<< (processCommand comm)

processCommand :: Command -> IO [Result]
processCommand = _processCommandWithCustomBuildPlanManager return

_processCommandWithCustomBuildPlanManager :: (BuildPlanManager -> IO BuildPlanManager) -> Command -> IO [Result]
_processCommandWithCustomBuildPlanManager customBPM comm = impl where
  impl = do
    bp_man <- customBPM =<< newBuildPlanManager (commBuildPlanDir comm) (commLogger comm) (commAllowNetwork comm)
    fmap concat $ mapM (processQueriesIn bp_man) $ commSources comm
  logger = commLogger comm
  processQueriesIn bp_man source = do
    queried_names <- fmap (nub . concat) $ mapM getQueriedPackageNames $ commQueries comm
    logDebug logger ("Retrieve package source " ++ show source)
    e_build_plan <- loadBuildPlan bp_man queried_names source
    logBuildPlanResult e_build_plan
    return $ map (makeResult source e_build_plan) $ commQueries comm
  makeResult source e_build_plan query = case e_build_plan of
    Left error_msg -> Result { resultIn = source, resultReallyIn = Nothing,
                               resultFor = query, resultBody = Left error_msg
                             }
    Right build_plan -> Result { resultIn = source,
                                 resultReallyIn = if source == real_source then Nothing else Just real_source,
                                 resultFor = query,
                                 resultBody = Right $ searchVersions build_plan query
                               }
      where real_source = buildPlanSource build_plan
  logBuildPlanResult (Right _) = logDebug logger ("Successfully retrieved build plan.")
  logBuildPlanResult (Left error_msg) = logError logger ("Failed to load build plan: " ++ error_msg)

searchVersions :: BuildPlan -> Query -> ResultBody
searchVersions build_plan (QueryName package_name) = SimpleResultBody package_name $ packageVersion build_plan package_name
searchVersions _ (QueryCabalFile _) = undefined -- TODO

getQueriedPackageNames :: Query -> IO [PackageName]
getQueriedPackageNames (QueryName n) = return [n]
getQueriedPackageNames (QueryCabalFile _) = undefined -- TODO
