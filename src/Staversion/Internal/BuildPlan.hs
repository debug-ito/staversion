-- |
-- Module: Staversion.Internal.BuildPlan
-- Description:  Handle build plan YAML files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Staversion.Internal.BuildPlan
       ( -- * Entry APIs
         BuildPlan,
         packageVersion,
         BuildPlanManager,
         newBuildPlanManager,
         loadBuildPlan,
         -- * Low-level APIs
         loadBuildPlanYAML,
         -- * For tests
         _setDisambiguator
       ) where

import Control.Applicative (empty, (<$>), (<*>))
import Control.Exception (throwIO, catchJust, IOException, catch)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mapM)
import Data.Aeson (FromJSON(..), (.:), Value(..), Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid (Monoid, (<>), mconcat)
import Data.Text (Text)
import Data.Traversable (Traversable(traverse))
import Data.Version (Version)
import qualified Data.Yaml as Yaml
import System.FilePath ((</>), (<.>))
import qualified System.IO.Error as IOE
import Text.Read (readMaybe)

import Staversion.Internal.Log
  ( Logger, logDebug, logWarn
  )
import Staversion.Internal.HTTP (niceHTTPManager, Manager, OurHttpException)
import Staversion.Internal.Query
 ( PackageName, PackageSource(..),
   ErrorMsg, Resolver
 )
import Staversion.Internal.BuildPlan.Hackage
  ( RegisteredVersions, latestVersion,
    fetchPreferredVersions
  )
import Staversion.Internal.BuildPlan.Stackage
  ( Disambiguator,
    fetchDisambiguator,
    parseResolverString,
    formatResolverString,
    PartialResolver(..), ExactResolver,
    fetchBuildPlanYAML
  )
import Staversion.Internal.BuildPlan.Version (unVersionJSON)

-- | A data structure that keeps a map between package names and their
-- versions.
newtype BuildPlan = BuildPlan (HM.HashMap PackageName Version) deriving (Monoid)

instance FromJSON BuildPlan where
  parseJSON (Object object) = (\p1 p2 -> BuildPlan $ p1 <> p2) <$> core_packages <*> other_packages where
    core_packages = parseSysInfo =<< (object .: "system-info")
    parseSysInfo (Object o) = parseCorePackages =<< (o .: "core-packages")
    parseSysInfo _ = empty
    parseCorePackages (Object o) = traverse (\v -> unVersionJSON <$> parseJSON v) o
    parseCorePackages _ = empty

    other_packages = parsePackages =<< (object .: "packages")
    parsePackages (Object o) = traverse parsePackageObject o
    parsePackages _ = empty
    parsePackageObject (Object o) = unVersionJSON <$> (o .: "version")
    parsePackageObject _ = empty
  parseJSON _ = empty

-- | Stateful manager for 'BuildPlan's.
data BuildPlanManager =
  BuildPlanManager { manBuildPlanDir :: FilePath,
                     -- ^ path to the directory where build plans are hold.
                     manHttpManager :: Maybe Manager,
                     -- ^ low-level HTTP connection manager. If 'Nothing', it won't fetch build plans over the network.
                     manDisambiguator :: IORef (Maybe Disambiguator),
                     -- ^ cache of resolver disambigutor
                     manLogger :: Logger
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
  return $ BuildPlanManager { manBuildPlanDir = plan_dir,
                              manHttpManager = mman,
                              manDisambiguator = disam,
                              manLogger = logger
                            }

type LoadM = ExceptT ErrorMsg IO

loggedElse :: Logger
           -> LoadM a -- ^ first action tried.
           -> LoadM a -- ^ the action executed if the first action returns 'Left'.
           -> LoadM a
loggedElse logger first second = ExceptT $ do
  eret <- runExceptT first
  case eret of
   Right _ -> return eret
   Left e -> logWarn logger e >> runExceptT second

maybeToLoadM :: ErrorMsg -> Maybe a -> LoadM a
maybeToLoadM msg = ExceptT . return . maybe (Left msg) Right

httpManagerM :: BuildPlanManager -> LoadM Manager
httpManagerM = maybeToLoadM "It is not allowed to access network." . manHttpManager

httpExceptionToLoadM :: String -> LoadM a -> LoadM a
httpExceptionToLoadM context action = ExceptT $ (runExceptT action) `catch` handler where
  handler :: OurHttpException -> IO (Either ErrorMsg a)
  handler e = return $ Left (context ++ ": " ++ show e)

loadBuildPlan :: BuildPlanManager
              -> [PackageName] -- ^ package names whose versions the user is interested in.
              -> PackageSource -> IO (Either ErrorMsg BuildPlan)
loadBuildPlan man _ (SourceStackage resolver) = runExceptT impl where
  impl = loadBuildPlan_stackageLocalFile man resolver `loggedElse'` do
    e_resolver <- tryDisambiguate man =<< getPresolver
    loadBuildPlan_stackageLocalFile man (formatResolverString $ PartialExact e_resolver) `loggedElse'` loadBuildPlan_stackageNetwork man e_resolver
  getPresolver = maybeToLoadM ("Invalid resolver format for stackage.org: " ++ resolver) $ parseResolverString resolver
  loggedElse' = loggedElse $ manLogger man
loadBuildPlan man names SourceHackage = runExceptT impl where
  impl = do
    http_man <- httpManagerM man
    (mconcat . zipWith registeredVersionToBuildPlan names) <$> (mapM (ExceptT . fetchPreferredVersions http_man) $ names)

loadBuildPlan_stackageLocalFile :: BuildPlanManager -> Resolver -> LoadM BuildPlan
loadBuildPlan_stackageLocalFile man resolver = ExceptT $ catchJust handleIOError doLoad (return . Left) where
  yaml_file = manBuildPlanDir man </> resolver <.> "yaml"
  doLoad = do
    logDebug (manLogger man) ("Read " ++ yaml_file ++ " for build plan.")
    loadBuildPlanYAML yaml_file
  handleIOError :: IOException -> Maybe ErrorMsg
  handleIOError e | IOE.isDoesNotExistError e = Just $ makeErrorMsg e (yaml_file ++ " not found.")
                  | IOE.isPermissionError e = Just $ makeErrorMsg e ("you cannot open " ++ yaml_file ++ ".")
                  | otherwise = Just $ makeErrorMsg e ("some error.")
  makeErrorMsg exception body = "Loading build plan for package resolver '" ++ resolver ++ "' failed: " ++ body ++ "\n" ++ show exception

tryDisambiguate :: BuildPlanManager -> PartialResolver -> LoadM ExactResolver
tryDisambiguate _ (PartialExact e) = return e
tryDisambiguate bp_man presolver = impl where
  impl = do
    disam <- httpExceptionToLoadM "Failed to download disambiguator" $ getDisambiguator
    maybeToLoadM ("Cannot disambiguate the resolver: " ++ show presolver) $ disam presolver
  getDisambiguator = do
    m_disam <- liftIO $ readIORef $ manDisambiguator bp_man
    case m_disam of
     Just d -> return d
     Nothing -> do
       http_man <- httpManagerM bp_man
       logDebug' "Fetch resolver disambiguator from network..."
       got_d <- ExceptT $ fetchDisambiguator http_man
       logDebug' "Successfully fetched resolver disambiguator."
       liftIO $ writeIORef (manDisambiguator bp_man) $ Just got_d
       return got_d
  logDebug' = liftIO . logDebug (manLogger bp_man)
  
loadBuildPlan_stackageNetwork :: BuildPlanManager -> ExactResolver -> LoadM BuildPlan
loadBuildPlan_stackageNetwork man e_resolver = do
  http_man <- httpManagerM man
  liftIO $ logDebug (manLogger man) ("Fetch build plan from network: resolver = " ++ show e_resolver)
  yaml_data <- httpExceptionToLoadM ("Downloading build plan failed: " ++ show e_resolver) $ liftIO $ fetchBuildPlanYAML http_man e_resolver
  ExceptT $ return $ parseBuildPlanYAML $ BSL.toStrict yaml_data

parseBuildPlanYAML :: BS.ByteString -> Either ErrorMsg BuildPlan
parseBuildPlanYAML = either (Left . toErrorMsg) Right  . Yaml.decodeEither' where
  toErrorMsg parse_exception = "Error while parsing BuildPlan YAML: " ++ show parse_exception

-- | Load a 'BuildPlan' from a file.
loadBuildPlanYAML :: FilePath -> IO (Either ErrorMsg BuildPlan)
loadBuildPlanYAML yaml_file = parseBuildPlanYAML <$> BS.readFile yaml_file where -- TODO: make it memory-efficient!

packageVersion :: BuildPlan -> PackageName -> Maybe Version
packageVersion (BuildPlan bp_map) name = HM.lookup name bp_map

_setDisambiguator :: BuildPlanManager -> Maybe Disambiguator -> IO ()
_setDisambiguator bp_man = writeIORef (manDisambiguator bp_man)

registeredVersionToBuildPlan :: PackageName -> RegisteredVersions -> BuildPlan
registeredVersionToBuildPlan name rvers = BuildPlan $ HM.fromList $ pairs where
  pairs = case latestVersion rvers of
    Nothing -> []
    Just v -> [(name, v)]
