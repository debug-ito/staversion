-- |
-- Module: Staversion.Internal.Command
-- Description: Command from the user.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Command
       ( Command(..),
         parseCommandArgs,
         queriesInCommand
       ) where

import Control.Applicative ((<$>), (<*>), optional, some)
import Data.Monoid (mconcat)
import Data.Text (pack)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_staversion as MyInfo
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Staversion.Internal.Log
  ( LogLevel(..), Logger(loggerThreshold), defaultLogger
  )
import Staversion.Internal.Query
  ( Resolver,
    PackageName,
    Query(..),
    PackageSource(..)
  )

-- | Command from the user.
data Command =
  Command { commBuildPlanDir :: FilePath,
            -- ^ path to the directory where build plan files are stored.
            commLogger :: Logger,
            -- ^ the logger
            commResolvers :: [Resolver],
            -- ^ stackage resolvers to search
            commPackages :: [PackageName]
            -- ^ package names to search for
          } deriving (Show,Eq)

-- | Default values for 'Command'.
data DefCommand = DefCommand { defBuildPlanDir :: FilePath
                             } deriving (Show,Eq,Ord)

defCommand :: IO DefCommand
defCommand = DefCommand <$> def_build_plan_dir where
  def_build_plan_dir = do
    home <- getHomeDirectory
    return $ home </> ".stack" </> "build-plan"


commandParser :: DefCommand -> Opt.Parser Command
commandParser def_comm = Command <$> build_plan_dir <*> logger <*> resolvers <*> packages where
  logger = makeLogger <$> is_verbose
  makeLogger True = defaultLogger { loggerThreshold = LogDebug }
  makeLogger False = defaultLogger
  is_verbose = Opt.switch $ mconcat [ Opt.long "verbose",
                                      Opt.short 'v',
                                      Opt.help "Verbose messages."
                                    ]
  build_plan_dir = Opt.strOption
                   $ mconcat [ Opt.long "build-plan-dir",
                               Opt.help "Directory where build plan YAML files are stored.",
                               Opt.metavar "DIR",
                               Opt.value (defBuildPlanDir def_comm)
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

programDescription :: Opt.Parser a -> Opt.ParserInfo a
programDescription parser =
  Opt.info (Opt.helper <*> parser)
  $ mconcat [ Opt.fullDesc,
              Opt.progDesc ( "Look for version numbers for Haskell packages in specific stackage resolvers"
                             ++ " (or possibly other package sources)"
                           ),
              Opt.footer ("Version: " ++ (showVersion MyInfo.version))
            ]

parseCommandArgs :: IO Command
parseCommandArgs = Opt.execParser . programDescription . commandParser =<< defCommand

queriesInCommand :: Command -> [Query]
queriesInCommand comm = do
  name <- commPackages comm
  resolver <- commResolvers comm
  return $ QueryName { querySource = SourceStackage resolver, queryName = name }
