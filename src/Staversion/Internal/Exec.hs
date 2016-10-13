-- |
-- Module: Staversion.Internal.Exec
-- Description: executable
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Exec
       ( main,
         processCommand
       ) where

import Control.Applicative ((<$>))
import Control.Exception (catchJust, IOException)
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (unpack)
import qualified Data.Text.Lazy.IO as TLIO
import System.FilePath ((</>), (<.>))
import qualified System.IO.Error as IOE

import Staversion.Internal.BuildPlan
  ( BuildPlan, loadBuildPlanYAML, packageVersion
  )
import Staversion.Internal.Command
  ( parseCommandArgs,
    Command(..)
  )
import Staversion.Internal.Format (formatResultsCabal)
import Staversion.Internal.Log (logDebug, logWarn)
import Staversion.Internal.Query
  ( Query(..), Result(..), PackageSource(..),
    resultVersionsFromList, ResultVersions,
    ErrorMsg
  )

main :: IO ()
main = do
  comm <- parseCommandArgs
  (TLIO.putStr . formatResultsCabal) =<< (processCommand comm)

processCommand :: Command -> IO [Result]
processCommand comm = fmap concat $ mapM processQueriesIn $ commSources comm where
  logger = commLogger comm
  processQueriesIn source = do
    logDebug logger ("Retrieve package source " ++ show source)
    e_build_plan <- loadBuildPlan comm source
    logBuildPlanResult e_build_plan
    return $ map (makeResult source e_build_plan) $ commQueries comm
  makeResult source e_build_plan query = case e_build_plan of
    Left error_msg -> Result { resultIn = source, resultFor = query, resultVersions = Left error_msg }
    Right build_plan -> Result { resultIn = source, resultFor = query,
                                 resultVersions = Right $ searchVersions build_plan query
                               }
  logBuildPlanResult (Right _) = logDebug logger ("Successfully retrieved build plan.")
  logBuildPlanResult (Left error_msg) = logWarn logger ("Failed to load build plan: " ++ error_msg)

loadBuildPlan ::  Command -> PackageSource -> IO (Either ErrorMsg BuildPlan)
loadBuildPlan comm source@(SourceStackage resolver) = catchJust handleIOError (Right <$> doLoad) (return . Left) where
  yaml_file = commBuildPlanDir comm </> resolver <.> "yaml"
  doLoad = do
    logDebug (commLogger comm) ("Read " ++ yaml_file ++ " for build plan.")
    loadBuildPlanYAML yaml_file
  handleIOError :: IOException -> Maybe ErrorMsg
  handleIOError e | IOE.isDoesNotExistError e = Just $ makeErrorMsg e (yaml_file ++ " not found.")
                  | IOE.isPermissionError e = Just $ makeErrorMsg e ("you cannot open " ++ yaml_file ++ ".")
                  | otherwise = Just $ makeErrorMsg e ("some error.")
  makeErrorMsg exception body = "Loading build plan for package source " ++ show source ++ " failed: " ++ body ++ "\n" ++ show exception


searchVersions :: BuildPlan -> Query -> ResultVersions
searchVersions build_plan (QueryName package_name) =
  resultVersionsFromList [(package_name, packageVersion build_plan package_name)]
