-- |
-- Module: Staversion.Internal.BuildPlan.Parser
-- Description: Common parsers for BuildPlan modules
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.BuildPlan.Parser
  ( parserVersion
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
