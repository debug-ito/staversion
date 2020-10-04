-- |
-- Module: Staversion.Internal.BuildPlan.Parser
-- Description: Common parsers for BuildPlan modules
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan.Parser
  ( parserVersion,
    manyTillWithEnd
  ) where

import Control.Applicative (optional)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Text (unpack)

import Staversion.Internal.Megaparsec (Parser)
import qualified Staversion.Internal.Megaparsec as P
import Staversion.Internal.Version (Version, parseVersionText)

parserVersion :: Parser Version
parserVersion = do
  vstr <- P.textSatisfying (\c -> isDigit c || c == '.')
  void $ optional $ P.char '*'
  case parseVersionText vstr of
    Nothing -> fail ("Cannot parse to a version: " ++ unpack vstr)
    Just v -> return v

manyTillWithEnd :: Parser a -> Parser end -> Parser ([a], end)
manyTillWithEnd pa pe = do
  as <- P.manyTill pa $ P.lookAhead $ P.try pe
  e <- pe
  return (as, e)
