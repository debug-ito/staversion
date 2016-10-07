-- |
-- Module: Staversion.Internal.Command
-- Description: Command from the user.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Command
       () where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Staversion.Internal.Query
  ( Resolver,
    PackageName
  )

-- | Command from the user.
data Command =
  Command { commBuildPlanDir :: FilePath,
            -- ^ path to the directory where build plan files are stored.
            commResolvers :: [Resolver],
            -- ^ stackage resolvers to search
            commPackages :: [PackageName]
            -- ^ package names to search for
          } deriving (Show,Eq,Ord)

defaultBuildPlanDir :: IO FilePath
defaultBuildPlanDir = do
  home <- getHomeDirectory
  return $ home </> ".stack" </> "build-plan"

