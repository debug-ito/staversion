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
processCommand comm = fmap concat $ mapM processQueriesIn $ commSources comm where
  processQueriesIn source = do
    build_plan <- loadBuildPlan comm source
    return $ map (searchVersion source build_plan) $ commQueries comm

-- | TODO: implement error handling
loadBuildPlan ::  Command -> PackageSource -> IO BuildPlan
loadBuildPlan comm (SourceStackage resolver) = loadBuildPlanYAML yaml_file where
  yaml_file = commBuildPlanDir comm </> unpack resolver <.> "yaml"

searchVersion :: PackageSource -> BuildPlan -> Query -> Result
searchVersion source build_plan query@(QueryName package_name) =
  Result { resultIn = source,
           resultFor = query,
           resultVersion = ret_version
         }
  where
    ret_version = packageVersion build_plan package_name
