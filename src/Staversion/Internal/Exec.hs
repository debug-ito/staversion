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
import System.FilePath ((</>), (<.>))
import qualified System.IO.Error as IOE

import Staversion.Internal.BuildPlan
  ( BuildPlan, loadBuildPlanYAML, packageVersion
  )
import Staversion.Internal.Command
  ( parseCommandArgs,
    Command(..)
  )
import Staversion.Internal.Query
  ( Query(..), Result(..), PackageSource(..),
    resultVersionsFromList, ResultVersions,
    ErrorMsg
  )

main :: IO ()
main = do
  comm <- parseCommandArgs
  (putStrLn . show) =<< (processCommand comm)

processCommand :: Command -> IO [Result]
processCommand comm = fmap concat $ mapM processQueriesIn $ commSources comm where
  processQueriesIn source = do
    e_build_plan <- loadBuildPlan comm source
    return $ map (makeResult source e_build_plan) $ commQueries comm
  makeResult source e_build_plan query = case e_build_plan of
    Left error_msg -> Result { resultIn = source, resultFor = query, resultVersions = Left error_msg }
    Right build_plan -> Result { resultIn = source, resultFor = query,
                                 resultVersions = Right $ searchVersions build_plan query
                               }

loadBuildPlan ::  Command -> PackageSource -> IO (Either ErrorMsg BuildPlan)
loadBuildPlan comm source@(SourceStackage resolver) = catchJust handleIOError (Right <$> loadBuildPlanYAML yaml_file) (return . Left) where
  yaml_file = commBuildPlanDir comm </> resolver <.> "yaml"
  handleIOError :: IOException -> Maybe ErrorMsg
  handleIOError e | IOE.isDoesNotExistError e = Just $ makeErrorMsg e (yaml_file ++ " not found.")
                  | IOE.isPermissionError e = Just $ makeErrorMsg e ("you cannot open " ++ yaml_file ++ ".")
                  | otherwise = Just $ makeErrorMsg e ("some error.")
  makeErrorMsg exception body = "Loading build plan for package source " ++ show source ++ " failed: " ++ body ++ "\n" ++ show exception


searchVersions :: BuildPlan -> Query -> ResultVersions
searchVersions build_plan (QueryName package_name) =
  resultVersionsFromList [(package_name, packageVersion build_plan package_name)]
