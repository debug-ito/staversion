-- |
-- Module: Staversion.Internal.Command
-- Description: Command from the user.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Command
       ( Command(..),
         parseCommandArgs
       ) where

import Control.Applicative ((<$>), (<*>), optional, some, (<|>))
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
    parseQuery,
    PackageSource(..)
  )

-- | Command from the user.
data Command =
  Command { commBuildPlanDir :: FilePath,
            -- ^ path to the directory where build plan files are stored.
            commLogger :: Logger,
            -- ^ the logger
            commSources :: [PackageSource],
            -- ^ package sources to search
            commQueries :: [Query],
            -- ^ package queries
            commAllowNetwork :: Bool
            -- ^ if 'True', it accesses the Internet to query build plans etc.
          } deriving (Show)

-- | Default values for 'Command'.
data DefCommand = DefCommand { defBuildPlanDir :: FilePath
                             } deriving (Show,Eq,Ord)

defCommand :: IO DefCommand
defCommand = DefCommand <$> def_build_plan_dir where
  def_build_plan_dir = do
    home <- getHomeDirectory
    return $ home </> ".stack" </> "build-plan"


commandParser :: DefCommand -> Opt.Parser Command
commandParser def_comm = Command <$> build_plan_dir <*> logger <*> sources <*> queries <*> network where
  logger = makeLogger <$> is_verbose
  makeLogger True = defaultLogger { loggerThreshold = Just LogDebug }
  makeLogger False = defaultLogger
  is_verbose = Opt.switch $ mconcat [ Opt.long "verbose",
                                      Opt.short 'v',
                                      Opt.help "Verbose messages."
                                    ]
  build_plan_dir = Opt.strOption
                   $ mconcat [ Opt.long "build-plan-dir",
                               Opt.help "Directory where build plan YAML files are stored.",
                               Opt.metavar "DIR",
                               Opt.value (defBuildPlanDir def_comm),
                               Opt.showDefault
                             ]
  sources = some $ resolver <|> hackage
  resolver = fmap SourceStackage $ Opt.strOption
             $ mconcat [ Opt.long "resolver",
                         Opt.short 'r',
                         Opt.help "Stackage resolver to search. e.g. \"lts-6.15\"",
                         Opt.metavar "RESOLVER_NAME"
                       ]
  hackage = Opt.flag' SourceHackage
            $ mconcat [ Opt.long "hackage",
                        Opt.short 'H',
                        Opt.help "Search hackage.org for the latest version."
                      ]
  queries = some $ parseQuery <$> (query_package <|> query_cabal)
  query_package = Opt.strArgument
                  $ mconcat [ Opt.help "Name of package whose version you want to check.",
                              Opt.metavar "PACKAGE_NAME"
                            ]
  query_cabal = Opt.strArgument
                $ mconcat [ Opt.help ".cabal file name. It checks versions of packages in build-deps lists.",
                            Opt.metavar "CABAL_FILEPATH"
                          ]
  network = not <$> no_network
  no_network = Opt.switch $ mconcat [ Opt.long "no-network",
                                      Opt.help "Forbid network access."
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
