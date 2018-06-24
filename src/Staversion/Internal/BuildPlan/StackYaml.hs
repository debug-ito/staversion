-- |
-- Module: Staversion.Internal.BuildPlan.StackYaml
-- Description: Get PackageSource from stack.yaml
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- This module is meant to be exposed only to
-- "Staversion.Internal.BuildPlan" and test modules.
module Staversion.Internal.BuildPlan.StackYaml
       ( readResolver,
         configLocation,
         configLocationFromText
       ) where

import Control.Applicative (empty, many, some)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Yaml (FromJSON(..), Value(..), (.:), decodeEither)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Process
  ( shell, readCreateProcess
  )
import Text.Megaparsec (runParser, Parsec)
import Text.Megaparsec.Char (satisfy, space)

import Staversion.Internal.Log (Logger)
import Staversion.Internal.Query (Resolver, ErrorMsg)

newtype Resolver' = Resolver' { unResolver' :: Resolver }
                  deriving (Show,Eq,Ord)

instance FromJSON Resolver' where
  parseJSON (Object o) = fmap Resolver' $ o .: "resolver"
  parseJSON _ = empty

-- | Read the @resolver@ field in stack.yaml.
readResolver :: FilePath -- ^ path to stack.yaml
             -> IO (Either ErrorMsg Resolver)
readResolver file = fmap (fmap unResolver' . decodeEither) $ BS.readFile file

-- | Get the path to stack.yaml that @stack@ uses as the current
-- config.
configLocation :: Logger
               -> String -- ^ shell command for @stack@
               -> IO (Either ErrorMsg FilePath)
configLocation logger command = fmap (configLocationFromText =<<) $ getProcessOutput logger command

-- TODO: コマンド実行に失敗した時の例外をキャッチする。あと、どうもstderrはinheritになっているな。

getProcessOutput :: Logger -> String -> IO (Either ErrorMsg Text)
getProcessOutput _ command = fmap (return . pack) $ readCreateProcess cmd ""
  where
    cmd = shell (command <> " path")

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
    parser :: Parsec () Text (Text,Text)
    parser = do
      space
      fname <- term
      void $ many $ satisfy isSep
      fval <- term
      return (fname, fval)
      where
        isSep c = c == ':' || isSpace c
        term = fmap T.pack $ some $ satisfy (not . isSep)
      
