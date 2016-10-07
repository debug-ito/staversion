-- |
-- Module: Staversion.Internal.Command
-- Description: Command from the user.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Command
       () where

import Control.Applicative ((<$>), (<*>), optional, some)
import Data.Monoid (mconcat)
import Data.Text (pack)
import qualified Options.Applicative as Opt
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Staversion.Internal.Query
  ( Resolver,
    PackageName
  )

-- | Command from the user.
data Command =
  Command { commBuildPlanDir :: Maybe FilePath,
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

commandParser :: Opt.Parser Command
commandParser = Command <$> build_plan_dir <*> resolvers <*> packages where
  build_plan_dir = optional $ Opt.strOption
                   $ mconcat [ Opt.long "build-plan-dir",
                               Opt.help "Directory where build plan YAML files are stored.",
                               Opt.metavar "DIR"
                             ]
  resolvers = some $ fmap pack $ Opt.strOption
              $ mconcat [ Opt.long "resolver",
                          Opt.short 'r',
                          Opt.help "Stackage resolver to search. e.g. \"lts-6.15\"",
                          Opt.metavar "RESOLVER_NAME"
                        ]
  packages = some $ fmap pack $ Opt.strArgument
             $ mconcat [ Opt.help "Name of package whose version you want to check.",
                         Opt.metavar "PACKAGE_NAME"
                       ]
  
