-- |
-- Module: Staversion.Internal.Cabal
-- Description: functions dealing with .cabal files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Cabal
       ( loadCabalFile,
         Target(..),
         BuildDepends(..)
       ) where

import Control.Applicative ((<*), (*>), (<|>), (<*>), many, some)
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad (void, mzero, forM)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit, toLower, isSpace)
import Data.List (intercalate, nub)
import Data.Monoid (mconcat)
import Data.Text (pack, Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Text as P

import Staversion.Internal.Query
  ( PackageName, ErrorMsg
  )

-- | Build target type.
data Target = TargetLibrary -- ^ the @library@ target.
            | TargetExecutable Text -- ^ the @executable NAME@ target.
            | TargetTestSuite Text -- ^ the @test-suite NAME@ target.
            | TargetBenchmark Text -- ^ the @benchmark NAME@ target.
            deriving (Show,Eq,Ord)

-- | A block of @build-depends:@.
data BuildDepends =
  BuildDepends { depsTarget :: Target,
                 depsPackages :: [PackageName]
               } deriving (Show,Eq,Ord)

loadCabalFile :: FilePath -> IO (Either ErrorMsg [BuildDepends])
loadCabalFile cabal_filepath = handleIOError $ first show <$> parseContent <$> readContent where
  readContent = TIO.readFile cabal_filepath
  parseContent = P.runParser (cabalParser <* P.eof) cabal_filepath
  handleIOError = Exception.handle h where
    h :: IOException -> IO (Either ErrorMsg [BuildDepends])
    h = return . Left . show

isLineSpace :: Char -> Bool
isLineSpace ' ' = True
isLineSpace '\t' = True
isLineSpace _ = False

isOpenBrace :: Char -> Bool
isOpenBrace = (== '{')

isCloseBrace :: Char -> Bool
isCloseBrace = (== '}')

isBrace :: Char -> Bool
isBrace c = isOpenBrace c || isCloseBrace c

lengthOf :: (Char -> Bool) -> P.Parser Int
lengthOf p = length <$> (many $ P.satisfy p)

indent :: P.Parser Int
indent = lengthOf isLineSpace

finishLine :: P.Parser ()
finishLine = P.eof <|> void P.eol

emptyLine :: P.Parser ()
emptyLine = indent *> (comment_line <|> void P.eol) where
  comment_line = (P.try $ P.string "--") *> P.manyTill P.anyChar P.eol *> pure ()

blockHeadLine :: P.Parser Target
blockHeadLine = target <* trail <* finishLine where
  trail = many $ P.satisfy $ \c -> isLineSpace c || isOpenBrace c
  target = target_lib <|> target_exe <|> target_test <|> target_bench
  target_lib = P.try (P.string' "library") *> pure TargetLibrary
  target_exe = TargetExecutable <$> targetNamed "executable"
  target_test = TargetTestSuite <$> targetNamed "test-suite"
  target_bench = TargetBenchmark <$> targetNamed "benchmark"
  targetNamed :: String -> P.Parser Text
  targetNamed target_type = P.try (P.string' target_type)
                            *> (some $ P.satisfy isLineSpace)
                            *> (fmap pack $ some $ P.satisfy (not . isSpace))

fieldStart :: Maybe String -- ^ expected field name. If Nothing, it just don't care.
           -> P.Parser (String, Int) -- ^ (lower-case field name, indent level)
fieldStart mexp_name = do
  level <- indent
  name <- nameParser <* indent <* P.char ':'
  return (map toLower name, level)
  where
    nameParser = case mexp_name of
      Nothing -> some $ P.satisfy $ \c -> not (isLineSpace c || c == ':')
      Just exp_name -> P.string' exp_name

fieldBlock :: P.Parser (String, Text) -- ^ (lower-case field name, block content)
fieldBlock = impl where
  impl = do
    (field_name, level) <- P.try $ do
      _ <- many $ (P.try emptyLine <|> P.try conditionalLine <|> P.try bracesOnlyLine)
      fieldStart Nothing
    field_trail <- P.manyTill P.anyChar finishLine
    rest <- remainingLines level
    let text_block = T.intercalate "\n" $ map pack (field_trail : rest)
    return (field_name, text_block)
  remainingLines field_indent_level = reverse <$> go [] where
    go cur_lines = (P.eof *> pure cur_lines) <|> foundSomething cur_lines
    foundSomething cur_lines = do
      void $ many $ P.try emptyLine
      this_level <- P.lookAhead indent
      if this_level <= field_indent_level
        then pure cur_lines
        else do
        _ <- indent
        this_line <- P.manyTill P.anyChar finishLine
        go (this_line : cur_lines)
  bracesOnlyLine = indent *> some braceAndSpace *> finishLine
  braceAndSpace = P.satisfy isBrace *> indent

buildDependsLine :: P.Parser [PackageName]
buildDependsLine = P.space *> (pname `P.endBy` ignored) where
  pname = pack <$> (some $ P.satisfy allowedChar)
  allowedChar '-' = True
  allowedChar '_' = True
  allowedChar c = isAlpha c || isDigit c
  ignored = P.manyTill P.anyChar finishItem *> P.space
  finishItem = P.eof <|> (void $ P.char ',')

conditionalLine :: P.Parser ()
conditionalLine = void $ leader *> (term "if" <|> term "else") *> P.manyTill P.anyChar finishLine where
  leader = many $ P.satisfy $ \c -> isLineSpace c || isCloseBrace c
  term :: String -> P.Parser ()
  term t = P.try (P.string' t *> P.lookAhead term_sep)
  term_sep = void $ P.satisfy $ \c -> isSpace c || isBrace c

targetBlock :: P.Parser BuildDepends
targetBlock = do
  target <- P.try blockHeadLine
  fields <- some fieldBlock
  let build_deps_blocks = map snd $ filter (("build-depends" ==) . fst) $ fields
  packages <- fmap (nub . concat) $ forM build_deps_blocks $ \block -> do
    either (fail . show) return $ P.runParser (buildDependsLine <* P.space <* P.eof) "build-depends" block
  return $ BuildDepends { depsTarget = target,
                          depsPackages = packages
                        }

cabalParser :: P.Parser [BuildDepends]
cabalParser = reverse <$> go [] where
  go cur_deps = targetBlockParsed cur_deps <|> (P.eof *> pure cur_deps) <|> ignoreLine cur_deps
  targetBlockParsed cur_deps = do
    new_dep <- targetBlock
    go (new_dep : cur_deps)
  ignoreLine cur_deps = P.manyTill P.anyChar finishLine *> go cur_deps
