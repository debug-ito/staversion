-- |
-- Module: Staversion.Internal.Command
-- Description: Command from the user.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Command
       ( Command(..),
         parseCommandArgs,
         defFormatConfig
       ) where

import Control.Applicative ((<$>), (<*>), optional, some, (<|>))
import Data.Function (on)
import Data.Monoid (mconcat, (<>))
import Data.Text (pack)
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import qualified Paths_staversion as MyInfo
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Staversion.Internal.Aggregate (Aggregator)
import qualified Staversion.Internal.Aggregate as Agg
import Staversion.Internal.Format (FormatConfig(..), FormatVersion)
import qualified Staversion.Internal.Format as Format
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
            commAllowNetwork :: Bool,
            -- ^ if 'True', it accesses the Internet to query build plans etc.
            commAggregator :: Maybe Aggregator,
            -- ^ if 'Just', do aggregation over the results.
            commFormatConfig :: FormatConfig
            -- ^ config for the formatter
          }

-- | Default values for 'Command'.
data DefCommand = DefCommand { defBuildPlanDir :: FilePath
                             } deriving (Show,Eq,Ord)

defCommand :: IO DefCommand
defCommand = DefCommand <$> def_build_plan_dir where
  def_build_plan_dir = do
    home <- getHomeDirectory
    return $ home </> ".stack" </> "build-plan"

defFormatConfig :: FormatConfig
defFormatConfig = FormatConfig { fconfFormatVersion = Format.formatVersionCabal
                               }

commandParser :: DefCommand -> Opt.Parser Command
commandParser def_comm = Command <$> build_plan_dir <*> logger <*> sources
                         <*> queries <*> network <*> aggregate <*> format_config where
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
                $ mconcat [ Opt.help "(EXPERIMENTAL) .cabal file name. It checks versions of packages in build-deps lists.",
                            Opt.metavar "CABAL_FILEPATH"
                          ]
  network = not <$> no_network
  no_network = Opt.switch $ mconcat [ Opt.long "no-network",
                                      Opt.help "Forbid network access."
                                    ]
  aggregate = optional $ Opt.option (maybeReader "AGGREGATOR" parseAggregator)
              $ mconcat [ Opt.long "aggregate",
                          Opt.short 'a',
                          Opt.metavar "AGGREGATOR",
                          Opt.helpDoc $ Just $ docAggregators "AGGREGATOR"
                        ]
  format_config = pure $ defFormatConfig

maybeReader :: String -> (String -> Maybe a) -> Opt.ReadM a
maybeReader metavar mfunc = do
  got <- Opt.str
  case mfunc got of
   Nothing -> Opt.readerError ("Unknown " ++ metavar ++ ": " ++ got)
   Just v -> return v


data SelectSpec a = SelectSpec { selectResult :: a,
                                 selectSymbol :: String,
                                 selectDesc :: String
                               }

type AggregatorSpec = SelectSpec Aggregator

aggregators :: [AggregatorSpec]
aggregators = [ SelectSpec Agg.aggOr "or" "concatenate versions with (||).",
                SelectSpec Agg.aggPvpMajor "pvp-major"
                ( "aggregate versions to a range that is supposed to be "
                  ++ "compatible with the given versions "
                  ++ "in terms of PVP (Package Versioning Policy.) "
                  ++ "Major versions are used for upper bounds."
                ),
                SelectSpec Agg.aggPvpMajor "pvp" "alias for 'pvp-major'",
                SelectSpec Agg.aggPvpMinor "pvp-minor"
                ( "aggregate versions to a range that is supposed to be "
                  ++ "compatible with the given versions "
                  ++ "in terms of PVP. "
                  ++ "Minor versions are used for upper bounds, i.e. this is stricter than 'pvp-major'."
                )
              ]

parseSelect :: [SelectSpec a] -> String -> Maybe a
parseSelect specs symbol = toMaybe $ filter (\spec -> selectSymbol spec == symbol) specs where
  toMaybe [] = Nothing
  toMaybe (spec : _) = Just $ selectResult spec

parseAggregator :: String -> Maybe Aggregator
parseAggregator = parseSelect aggregators

wrapped :: String -> Pretty.Doc
wrapped = Pretty.fillSep . map Pretty.text . words

docSelect :: [SelectSpec a] -> String -> String -> Pretty.Doc
docSelect specs foreword_str metavar = Pretty.vsep $ (foreword  :) $ map docSpec specs where
  foreword = wrapped ( foreword_str ++ " Possible " ++ metavar ++ " is:" )
  docSpec SelectSpec {selectSymbol = symbol, selectDesc = desc} =
    Pretty.hang 2 $ wrapped ("\"" <> symbol <> "\": " <> desc)

docAggregators :: String -> Pretty.Doc
docAggregators = docSelect aggregators "Aggregate version results over different resolvers."


formatVersions :: [SelectSpec FormatVersion]
formatVersions = [ SelectSpec Format.formatVersionCabal "cabal"
                   ( "Let Cabal format VersionRanges"
                   )
                 ]

docFormatVersions :: String -> Pretty.Doc
docFormatVersions = docSelect formatVersions "Format for package version ranges."

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
