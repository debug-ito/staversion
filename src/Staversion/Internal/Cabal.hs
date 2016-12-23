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
import Control.Monad (void, mzero)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (lookup, intercalate)
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
loadCabalFile cabal_filepath = first show <$> P.runParser (cabalParser <* P.eof) cabal_filepath <$> TIO.readFile cabal_filepath

isLineSpace :: Char -> Bool
isLineSpace ' ' = True
isLineSpace '\t' = True
isLineSpace _ = False

indent :: P.Parser Int
indent = length <$> (many $ P.satisfy isLineSpace)

finishLine :: P.Parser ()
finishLine = P.eof <|> void P.eol

emptyLine :: P.Parser ()
emptyLine = indent *> (P.try finishLine <|> comment_line) where
  comment_line = P.string "--" *> P.manyTill P.anyChar finishLine *> pure ()

blockHeadLine :: P.Parser Target
blockHeadLine = indent *> target <* trail <* finishLine where
  trail = indent
  target = target_lib <|> target_exe <|> target_test <|> target_bench
  target_lib = P.try (P.string' "library") *> pure TargetLibrary
  target_exe = TargetExecutable <$> targetNamed "executable"
  target_test = TargetTestSuite <$> targetNamed "test-suite"
  target_bench = TargetBenchmark <$> targetNamed "benchmark"
  targetNamed :: String -> P.Parser Text
  targetNamed target_type = P.try (P.string' target_type)
                            *> (some $ P.satisfy isLineSpace)
                            *> (fmap pack $ some $ P.satisfy (not . isLineSpace))

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
    (field_name, level) <- P.try $ fieldStart Nothing
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

buildDependsLine :: P.Parser [PackageName]
buildDependsLine = pname `P.sepBy` P.char ',' where
  pname = pack <$> pname_str
  pname_str = P.space *> (some $ P.satisfy allowedChar) <* (P.manyTill P.anyChar $ P.satisfy (\c -> c == ','))
  allowedChar '-' = True
  allowedChar '_' = True
  allowedChar c = isAlpha c || isDigit c

targetBlock :: P.Parser BuildDepends
targetBlock = do
  target <- P.try blockHeadLine
  _ <- many $ P.try emptyLine
  fields <- some fieldBlock
  build_deps_block <- maybe mzero return $ lookup "build-depends" fields
  packages <- either (fail . show) return $ P.runParser (buildDependsLine <* P.space <* P.eof) "build-depends" build_deps_block
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

-- cabalParser = reverse <$> go [] where
--   go cur_deps = do
--     mret <- (Just <$> targetBlock) <|> (ignoreLine *> pure Nothing)
--     case mret of
--      Just b -> go (b : cur_deps)
--      Nothing -> go cur_deps
--   ignoreLine = P.manyTill anyChar finishLine

-- cabalParser = impl where
--   impl = ((:) <$> targetBlock <*> impl) <|> ignoreLine
--   ignoreLine = do
--     _ <- P.takeWhile (not . P.isEndOfLine)
--     (P.endOfInput *> pure []) <|> (P.endOfLine *> impl)
