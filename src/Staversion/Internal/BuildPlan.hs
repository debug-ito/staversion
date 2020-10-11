-- |
-- Module: Staversion.Internal.BuildPlan
-- Description:  Handle build plan YAML files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Staversion.Internal.BuildPlan
       ( -- * Entry APIs
         HasVersions(..),
         BuildPlan,
         buildPlanSource,
         BuildPlanManager,
         newBuildPlanManager,
         manStackConfig,
         loadBuildPlan,
         -- * Low-level APIs
         BuildPlanMap,
         -- * For tests
         _setLTSDisambiguator
       ) where

import Control.Applicative (empty, (<$>), (<*>))
import Control.Exception (throwIO, catchJust, IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mapM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid (Monoid, (<>), mconcat)
import Data.Semigroup (Semigroup)
import Data.Text (Text, unpack)
import Data.Traversable (Traversable(traverse))
import Data.Word (Word)
import System.FilePath ((</>), (<.>))
import qualified System.IO.Error as IOE
import Text.Read (readMaybe)

import Staversion.Internal.EIO
  ( EIO, maybeToEIO, runEIO, toEIO, loggedElse, eitherToEIO
  )
import Staversion.Internal.Log
  ( Logger, logDebug, logWarn
  )
import Staversion.Internal.HTTP (niceHTTPManager, Manager, OurHttpException)
import Staversion.Internal.Query
 ( PackageName, PackageSource(..),
   ErrorMsg, Resolver
 )
import Staversion.Internal.BuildPlan.BuildPlanMap
  ( BuildPlanMap, HasVersions(..)
  )
import qualified Staversion.Internal.BuildPlan.BuildPlanMap as BPMap
import Staversion.Internal.BuildPlan.Core (CompilerCores)
import qualified Staversion.Internal.BuildPlan.Core as Core
import Staversion.Internal.BuildPlan.Hackage
  ( RegisteredVersions, latestVersion,
    fetchPreferredVersions
  )
import qualified Staversion.Internal.BuildPlan.Pantry as Pantry
import Staversion.Internal.BuildPlan.Stackage
  ( Disambiguator,
    fetchDisambiguator,
    parseResolverString,
    formatExactResolverString,
    PartialResolver(..), ExactResolver(..)
  )
import Staversion.Internal.BuildPlan.V1 as V1
import Staversion.Internal.StackConfig (StackConfig)
import qualified Staversion.Internal.StackConfig as StackConfig
import Staversion.Internal.Version (Version)


-- | A 'BuildPlanMap' associated with its 'PackageSource'.
data BuildPlan = BuildPlan { buildPlanMap :: BuildPlanMap,
                             buildPlanSource :: PackageSource
                           }

instance HasVersions BuildPlan where
  packageVersion bp = packageVersion (buildPlanMap bp)


-- | Stateful manager for 'BuildPlan's.
data BuildPlanManager =
  BuildPlanManager { manBuildPlanDir :: FilePath,
                     -- ^ (accessor function) path to the directory
                     -- where build plans are hold.
                     manHttpManager :: Maybe Manager,
                     -- ^ (accessor function) low-level HTTP
                     -- connection manager. If 'Nothing', it won't
                     -- fetch build plans over the network.
                     manDisambiguator :: IORef (Maybe Disambiguator),
                     -- ^ (accessor function) cache of resolver
                     -- disambigutor
                     manCores :: IORef (Maybe CompilerCores),
                     -- ^ cache of compiler core packages.
                     manLogger :: Logger,
                     manStackConfig :: StackConfig
                     -- ^ (accessor function)
                   }

newBuildPlanManager :: FilePath -- ^ path to the directory where build plans are hold.
                    -> Logger
                    -> Bool -- ^ If 'True', it queries the Internet for build plans. Otherwise, it won't.
                    -> IO BuildPlanManager
newBuildPlanManager plan_dir logger enable_network = do
  mman <- if enable_network
          then Just <$> niceHTTPManager
          else return Nothing
  disam <- newIORef Nothing
  cores <- newIORef Nothing
  return $ BuildPlanManager { manBuildPlanDir = plan_dir,
                              manHttpManager = mman,
                              manDisambiguator = disam,
                              manCores = cores,
                              manLogger = logger,
                              manStackConfig = StackConfig.newStackConfig logger
                            }

httpManagerM :: BuildPlanManager -> EIO Manager
httpManagerM = maybeToEIO "It is not allowed to access network." . manHttpManager

httpExceptionToEIO :: String -> EIO a -> EIO a
httpExceptionToEIO context action = toEIO $ (runEIO action) `catch` handler where
  handler :: OurHttpException -> IO (Either ErrorMsg a)
  handler e = return $ Left (context ++ ": " ++ show e)

getCores :: BuildPlanManager -> EIO CompilerCores
getCores man = do
  mcores <- liftIO $ readIORef $ manCores man
  case mcores of
    Just c -> return c
    Nothing -> do
      http <- httpManagerM man
      liftIO $ logDebug (manLogger man) "fetching GHC pkg_versions"
      cores <- httpExceptionToEIO "Failed to fetch GHC pkg_versions"
               $ toEIO $ fmap Core.parseGHCPkgVersions $ Core.fetchGHCPkgVersions http
      liftIO $ writeIORef (manCores man) $ Just cores
      return cores

loadBuildPlan :: BuildPlanManager
              -> [PackageName]
              -- ^ package names whose versions the user is interested in.
              -> PackageSource
              -> IO (Either ErrorMsg BuildPlan)
              -- ^ the second result is the real (disambiguated) PackageSource.
loadBuildPlan man names s = runEIO $ loadBuildPlanM man names s

loadBuildPlanM :: BuildPlanManager -> [PackageName] -> PackageSource -> EIO BuildPlan
loadBuildPlanM man _ (SourceStackage resolver) = impl where
  impl = loadBuildPlan_stackageLocalFile man resolver `loggedElse'` do
    e_resolver <- tryDisambiguate man =<< getPresolver
    loadBuildPlan_stackageLocalFile man (formatExactResolverString e_resolver) `loggedElse'` loadBuildPlan_stackageNetwork man e_resolver
  getPresolver = maybeToEIO ("Invalid resolver format for stackage.org: " ++ resolver) $ parseResolverString resolver
  loggedElse' = loggedElse $ manLogger man
loadBuildPlanM man names SourceHackage = impl where
  impl = do
    http_man <- httpManagerM man
    build_plan_map <- (mconcat . zipWith registeredVersionToBuildPlanMap names) <$> mapM (doFetch http_man) names
    return $ BuildPlan { buildPlanMap = build_plan_map, buildPlanSource = SourceHackage }
  logDebug' msg = liftIO $ logDebug (manLogger man) msg
  logWarn' msg = liftIO $ logWarn (manLogger man) msg
  doFetch http_man name = do
    logDebug' ("Ask hackage for the latest version of " ++ unpack name)
    reg_ver <- toEIO $ fetchPreferredVersions http_man name
    case latestVersion reg_ver of
     Nothing -> logWarn' ("Cannot find package version of " ++ unpack name ++ ". Maybe it's not on hackage.")
     Just _ -> return ()
    return reg_ver
loadBuildPlanM man names (SourceStackYaml file) = loadBuildPlan_sourceStack man names $ Just file
loadBuildPlanM man names SourceStackDefault = loadBuildPlan_sourceStack man names $ Nothing

loadBuildPlan_sourceStack :: BuildPlanManager -> [PackageName] -> Maybe FilePath -> EIO BuildPlan
loadBuildPlan_sourceStack man names mfile = do
  resolver <- toEIO $ StackConfig.readResolver sconf mfile
  loadBuildPlanM man names $ SourceStackage resolver
  where
    sconf = manStackConfig man

loadBuildPlan_stackageLocalFile :: BuildPlanManager -> Resolver -> EIO BuildPlan
loadBuildPlan_stackageLocalFile man resolver = toEIO $ catchJust handleIOError doLoad (return . Left) where
  yaml_file = manBuildPlanDir man </> resolver <.> "yaml"
  doLoad = do
    logDebug (manLogger man) ("Read " ++ yaml_file ++ " for build plan.")
    e_build_plan_map <- V1.loadBuildPlanMapYAML yaml_file
    return $ makeBuildPlan <$> e_build_plan_map
  makeBuildPlan bp_map = BuildPlan { buildPlanMap = bp_map, buildPlanSource = SourceStackage resolver }
  handleIOError :: IOException -> Maybe ErrorMsg
  handleIOError e | IOE.isDoesNotExistError e = Just $ makeErrorMsg e (yaml_file ++ " not found.")
                  | IOE.isPermissionError e = Just $ makeErrorMsg e ("you cannot open " ++ yaml_file ++ ".")
                  | otherwise = Just $ makeErrorMsg e ("some error.")
  makeErrorMsg exception body = "Loading build plan for package resolver '" ++ resolver ++ "' failed: " ++ body ++ "\n" ++ show exception

tryDisambiguate :: BuildPlanManager -> PartialResolver -> EIO ExactResolver
tryDisambiguate _ (PartialExact e) = return e
tryDisambiguate bp_man presolver = impl where
  impl = do
    disam <- httpExceptionToEIO "Failed to download disambiguator" $ getDisambiguator
    maybeToEIO ("Cannot disambiguate the resolver: " ++ show presolver) $ disam presolver
  getDisambiguator = do
    m_disam <- liftIO $ readIORef $ manDisambiguator bp_man
    case m_disam of
     Just d -> return d
     Nothing -> do
       http_man <- httpManagerM bp_man
       logDebug' "Fetch resolver disambiguator from network..."
       got_d <- toEIO $ fetchDisambiguator http_man
       logDebug' "Successfully fetched resolver disambiguator."
       liftIO $ writeIORef (manDisambiguator bp_man) $ Just got_d
       return got_d
  logDebug' = liftIO . logDebug (manLogger bp_man)
  
loadBuildPlan_stackageNetwork :: BuildPlanManager -> ExactResolver -> EIO BuildPlan
loadBuildPlan_stackageNetwork man e_resolver = do
  cores <- getCores man
  http_man <- httpManagerM man
  liftIO $ logDebug (manLogger man) ("Fetch build plan from network: resolver = " ++ show e_resolver)
  yaml_data <- httpExceptionToEIO ("Downloading build plan failed: " ++ show e_resolver)
               $ liftIO $ Pantry.fetchBuildPlanMapYAML http_man e_resolver
  bp_map <- eitherToEIO $ (Pantry.coresToBuildPlanMap cores) =<< (Pantry.parseBuildPlanMapYAML $ BSL.toStrict yaml_data)
  return $ BuildPlan { buildPlanMap = bp_map,
                       buildPlanSource = SourceStackage $ formatExactResolverString e_resolver
                     }
registeredVersionToBuildPlanMap :: PackageName -> RegisteredVersions -> BuildPlanMap
registeredVersionToBuildPlanMap name rvers = BPMap.fromList $ pairs where
  pairs = case latestVersion rvers of
    Nothing -> []
    Just v -> [(name, v)]

_setDisambiguator :: BuildPlanManager -> Maybe Disambiguator -> IO ()
_setDisambiguator bp_man = writeIORef (manDisambiguator bp_man)

_setLTSDisambiguator :: BuildPlanManager
                     -> Word -- ^ disambiguated LTS major version
                     -> Word -- ^ disambiguated LTS minor version
                     -> IO ()
_setLTSDisambiguator bp_man lts_major lts_minor = _setDisambiguator bp_man $ Just disam where
  disam PartialLTSLatest = Just $ ExactLTS lts_major lts_minor
  disam _ = Nothing
