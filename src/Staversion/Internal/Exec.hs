-- |
-- Module: Staversion.Internal.Exec
-- Description: executable
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.Exec
       ( main
       ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.Text (unpack)
import System.FilePath ((</>), (<.>))

import Staversion.Internal.BuildPlan
  ( BuildPlan, loadBuildPlanYAML, packageVersion
  )
import Staversion.Internal.Command
  ( parseCommandArgs,
    queriesInCommand,
    Command(..)
  )
import Staversion.Internal.Query
  ( Query(..), Result(..), PackageSource(..)
  )

main :: IO ()
main = do
  comm <- parseCommandArgs
  (putStrLn . show) =<< (processCommand comm)

processCommand :: Command -> IO [Result]
processCommand comm = fmap concat $ mapM processGroupedQueries $ groupBy ((==) `on` querySource) $ queriesInCommand comm where
  processGroupedQueries [] = return []
  processGroupedQueries queries@(first_q : _) = processQueriesIn comm (querySource first_q) queries

processQueriesIn :: Command -> PackageSource -> [Query] -> IO [Result]
processQueriesIn comm source queries = do
  build_plan <- loadBuildPlan comm source
  return $ map (searchVersion build_plan) queries

-- | TODO: implement error handling
loadBuildPlan ::  Command -> PackageSource -> IO BuildPlan
loadBuildPlan comm (SourceStackage resolver) = loadBuildPlanYAML yaml_file where
  yaml_file = commBuildPlanDir comm </> unpack resolver <.> "yaml"

searchVersion :: BuildPlan -> Query -> Result
searchVersion build_plan query@(QueryName _ _) =
  Result { resultFor = query,
           resultVersion = ret_version
         }
  where
    ret_version = packageVersion build_plan (queryName query)
