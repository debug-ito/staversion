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
import Data.Char (isAlpha, isDigit)
import Data.List (lookup)
import Data.Text (pack, Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P (lookAhead)

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
loadCabalFile cabal_filepath = P.parseOnly (cabalParser <* P.endOfInput) <$> TIO.readFile cabal_filepath

isLineSpace :: Char -> Bool
isLineSpace ' ' = True
isLineSpace '\t' = True
isLineSpace _ = False

indent :: P.Parser Int
indent = T.length <$> P.takeWhile isLineSpace

finishLine :: P.Parser ()
finishLine = P.endOfLine <|> P.endOfInput

emptyLine :: P.Parser ()
emptyLine = true_empty <|> comment_line where
  true_empty = indent *> finishLine
  comment_line = indent *> P.string "--" *> P.takeTill P.isEndOfLine *> finishLine

blockHeadLine :: P.Parser Target
blockHeadLine = indent *> target <* trail <* finishLine where
  trail = indent
  target = target_lib <|> target_exe <|> target_test <|> target_bench
  target_lib = P.asciiCI "library" *> pure TargetLibrary
  target_exe = TargetExecutable <$> targetNamed "executable"
  target_test = TargetTestSuite <$> targetNamed "test-suite"
  target_bench = TargetBenchmark <$> targetNamed "benchmark"
  targetNamed target_type = P.asciiCI target_type *> P.takeWhile1 isLineSpace *> P.takeWhile1 (not . isLineSpace)

fieldStart :: Maybe Text -- ^ expected field name. If Nothing, it just don't care.
           -> P.Parser (Text, Int) -- ^ (lower-case field name, indent level)
fieldStart mexp_name = do
  level <- indent
  name <- nameParser <* indent <* P.char ':'
  return (T.toLower name, level)
  where
    nameParser = case mexp_name of
      Nothing -> P.takeWhile1 $ \c -> not (isLineSpace c || c == ':')
      Just exp_name -> P.asciiCI exp_name

fieldBlock :: P.Parser (Text, Text) -- ^ (lower-case field name, block content)
fieldBlock = impl where
  impl = do
    (field_name, level) <- fieldStart Nothing
    field_trail <- P.takeTill P.isEndOfLine <* finishLine
    rest <- remainingLines level
    return (field_name, T.intercalate "\n" (field_trail : rest))
  remainingLines field_indent_level = go where
    go = (emptyLine *> go) <|> (P.endOfInput *> pure []) <|> foundSomething
    foundSomething = do
      this_level <- P.lookAhead indent
      if this_level <= field_indent_level
        then pure []
        else do
        _ <- indent
        this_line <- P.takeTill (P.isEndOfLine) <* finishLine
        (this_line :) <$> go

buildDependsLine :: P.Parser [PackageName]
buildDependsLine = pname `P.sepBy` P.char ',' where
  pname = P.skipSpace *> P.takeWhile1 allowedChar <* P.takeTill (\c -> c == ',')
  allowedChar '-' = True
  allowedChar '_' = True
  allowedChar c = isAlpha c || isDigit c

targetBlock :: P.Parser BuildDepends
targetBlock = do
  target <- blockHeadLine
  _ <- many emptyLine
  fields <- some fieldBlock
  build_deps_block <- maybe mzero return $ lookup "build-depends" fields
  packages <- either (const mzero) return $ P.parseOnly (buildDependsLine <* P.skipSpace <* P.endOfInput) build_deps_block
  return $ BuildDepends { depsTarget = target,
                          depsPackages = packages
                        }

cabalParser :: P.Parser [BuildDepends]
cabalParser = impl where
  impl = ((:) <$> targetBlock <*> impl) <|> (ignoreLine *> impl) <|> (P.endOfInput *> pure [])
  ignoreLine = P.takeTill P.isEndOfLine *> finishLine
