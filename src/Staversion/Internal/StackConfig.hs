-- |
-- Module: Staversion.Internal.StackConfig
-- Description: Central entity that deals with stack.yaml
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.StackConfig
       ( -- * StackConfig
         StackConfig,
         newStackConfig,
         scCommand,
         readResolver,
         readProjectCabals,
         -- * For tests
         configLocationFromText
       ) where

import Control.Applicative (empty, many, some, (<$>), (<*>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Data.Yaml (FromJSON(..), Value(..), (.:), decodeEither)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((</>), takeDirectory)
import System.Process
  ( shell, readCreateProcessWithExitCode
  )

import Staversion.Internal.EIO (EIO, toEIO, runEIO, eitherToEIO)
import Staversion.Internal.Log (Logger, logWarn, logDebug)
import Staversion.Internal.Query (Resolver, ErrorMsg)
import Staversion.Internal.Megaparsec (Parser, runParser, satisfy, space)


-- | Central entity that deals with stack.yaml and @stack@ command.
data StackConfig =
  StackConfig
  { scCommand :: String,
    -- ^ (accessor) shell command for @stack@ tool.
    scLogger :: Logger
  }

newStackConfig :: Logger -> StackConfig
newStackConfig = StackConfig "stack"

-- | Element of @packages@ field. If the path is for the main project
-- (i.e. @extra-dep@ is false), it's 'Just'. Otherwise, it's
-- 'Nothing'.
newtype ProjectPath = ProjectPath (Maybe FilePath)
                      deriving (Show,Eq,Ord)

instance FromJSON ProjectPath where
  parseJSON (String s) = return $ ProjectPath $ Just $ T.unpack s
  parseJSON (Object _) = return $ ProjectPath $ Nothing
  parseJSON _ = empty

-- | @stack.yaml@ content
data StackYaml =
  StackYaml
  { stackYamlPath :: FilePath,
    stackYamlResolver :: Resolver,
    stackYamlPackages :: [ProjectPath]
  }
  deriving (Show,Eq,Ord)

instance FromJSON StackYaml where
  parseJSON (Object o) = StackYaml "" <$> (o .: "resolver") <*> (o .: "packages")
  parseJSON _ = empty

readStackYaml :: FilePath -> EIO StackYaml
readStackYaml file = toEIO $ fmap (fmap setPath . decodeEither) $ BS.readFile file
  where
    setPath sy = sy { stackYamlPath = file }

findProjectCabal :: Logger -> FilePath -> ProjectPath -> IO [FilePath]
findProjectCabal _ _ (ProjectPath Nothing) = return []
findProjectCabal logger base_path (ProjectPath (Just project_path)) = do
  all_files <- getDirectoryContents project_fullpath
  let result_files = map (\f -> project_fullpath </> f) $ filter isCabalFile all_files
  when (length result_files == 0) $ do
    logWarn logger ("No .cabal file is found in " <> project_fullpath)
  return result_files
  where
    project_fullpath = base_path </> project_path
    isCabalFile f = ".cabal" `isSuffixOf` f

findProjectCabals :: Logger
                  -> StackYaml
                  -> IO [FilePath] -- ^ paths to all project .cabal files.
findProjectCabals logger stack_yaml = do
  cabals <- fmap concat $ mapM (findProjectCabal logger base_path) packages
  warnEmpty cabals
  return cabals
  where
    stack_yaml_path = stackYamlPath stack_yaml
    base_path = takeDirectory $ stack_yaml_path
    packages = stackYamlPackages stack_yaml
    warnEmpty [] = logWarn logger ("No project .cabal files found in " <> stack_yaml_path)
    warnEmpty _ = return ()

readProjectCabals :: StackConfig
                  -> Maybe FilePath
                  -- ^ path to stack.yaml. If 'Nothing', the default stack.yaml is used.
                  -> IO (Either ErrorMsg [FilePath])
                  -- ^ paths to all .cabal files of the stack projects.
readProjectCabals s f = runEIO $ readProjectCabalsEIO s f

readProjectCabalsEIO :: StackConfig -> Maybe FilePath -> EIO [FilePath]
readProjectCabalsEIO sconf (Just stack_yaml_file) = do
  stack_yaml <- readStackYaml stack_yaml_file
  liftIO $ findProjectCabals logger stack_yaml
  where
    logger = scLogger sconf
readProjectCabalsEIO sconf Nothing = do
  stack_yaml_file <- configLocation sconf
  readProjectCabalsEIO sconf $ Just stack_yaml_file

-- | Read the @resolver@ field in stack.yaml.
readResolver :: StackConfig
             -> Maybe FilePath
             -- ^ path to stack.yaml. If 'Nothing', the default stack.yaml is used.
             -> IO (Either ErrorMsg Resolver)
readResolver sconf mfile = runEIO $ case mfile of
  Just file -> doRead file
  Nothing -> doRead =<< configLocation sconf
  where
    doRead file = fmap stackYamlResolver $ readStackYaml file

-- | Get the path to stack.yaml that @stack@ uses as the current
-- config.
configLocation :: StackConfig -> EIO FilePath
configLocation sconfig = do
  pout <- getProcessOutput sconfig
  path <- eitherToEIO $ configLocationFromText pout
  liftIO $ logDebug logger ("Project stack config: " <> path)
  return path
  where
    logger = scLogger sconfig

getProcessOutput :: StackConfig -> EIO Text
getProcessOutput sconfig = toEIO $ handleResult =<< readCreateProcessWithExitCode cmd ""
  where
    logger = scLogger sconfig
    command = scCommand sconfig
    cmd_str = command <> " path"
    cmd = shell cmd_str
    warnErr err = when (length err /= 0) $ logWarn logger err
    handleResult (code, out, err) = do
      case code of
       ExitFailure c -> do
         let code_err = "'" <> cmd_str <> "' returns non-zero exit code: " <> show c <> "."
             hint = "It requires the 'stack' tool. Maybe you have to specify the command by --stack-command option."
         logWarn logger code_err
         warnErr err
         return $ Left (code_err <> "\n" <> hint)
       _ -> do
         warnErr err
         return $ Right $ pack out

configLocationFromText :: Text -> Either ErrorMsg FilePath
configLocationFromText input = toEither $ findField =<< T.lines input
  where
    fieldName = "config-location"
    findField :: Text -> [FilePath]
    findField line = do
      (fname, fvalue) <- maybe [] return $ parseField line
      if fname == fieldName
        then return $ T.unpack fvalue
        else []
    toEither :: [FilePath] -> Either ErrorMsg FilePath
    toEither [] = Left ("Cannot find '" <> T.unpack fieldName <> "' field in stack path")
    toEither (r:_) = Right r
    parseField :: Text -> Maybe (Text, Text)
    parseField = either (const Nothing) return . runParser parser ""
    parser :: Parser (Text,Text)
    parser = do
      space
      fname <- term
      void $ many $ satisfy isSep
      fval <- term
      return (fname, fval)
      where
        isSep c = c == ':' || isSpace c
        term = fmap T.pack $ some $ satisfy (not . isSep)
      
