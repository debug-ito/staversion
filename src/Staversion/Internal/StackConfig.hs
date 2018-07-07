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
         configLocation,
         -- * For tests
         configLocationFromText
       ) where

import Control.Applicative (empty, many, some, (<$>), (<*>))
import Control.Monad (void, when)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Yaml (FromJSON(..), Value(..), (.:), decodeEither)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Exit (ExitCode(ExitFailure))
import System.Process
  ( shell, readCreateProcessWithExitCode
  )

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
newtype ProjectPath = ProjectPath { unProjectPath :: Maybe FilePath }
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

readStackYaml :: FilePath -> IO (Either ErrorMsg StackYaml)
readStackYaml file = fmap (fmap setPath . decodeEither) $ BS.readFile file
  where
    setPath sy = sy { stackYamlPath = file }

findProjectCabals :: Logger -> StackYaml
                  -> IO [FilePath] -- ^ paths to all project .cabal files.
findProjectCabals = undefined

readProjectCabals :: StackConfig
                  -> Maybe FilePath
                  -- ^ path to stack.yaml. If 'Nothing', the default stack.yaml is used.
                  -> IO (Either ErrorMsg [FilePath])
                  -- ^ paths to all .cabal files of the stack projects.
readProjectCabals = undefined

-- | Read the @resolver@ field in stack.yaml.
readResolver :: FilePath -- ^ path to stack.yaml
             -> IO (Either ErrorMsg Resolver)
readResolver file = (fmap . fmap) stackYamlResolver $ readStackYaml file

-- | Get the path to stack.yaml that @stack@ uses as the current
-- config.
configLocation :: StackConfig -> IO (Either ErrorMsg FilePath)
configLocation sconfig = do
  pout <- getProcessOutput sconfig
  case configLocationFromText =<< pout of
   e@(Right path) -> logDebug logger ("Project stack config: " <> path) >> return e
   e -> return e
  where
    logger = scLogger sconfig

getProcessOutput :: StackConfig -> IO (Either ErrorMsg Text)
getProcessOutput sconfig = handleResult =<< readCreateProcessWithExitCode cmd ""
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
      
