{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Staversion.Internal.BuildPlan.Core
-- Description: Build plan of core packages (those bundled with a compiler)
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- @since 0.2.4.0
module Staversion.Internal.BuildPlan.Core
  ( -- * Types
    CompilerCores,
    CoreBuildPlanMap(..),
    Compiler(..),
    CompilerVersion(..),
    CompilerName,
    -- * Versions
    mkCompilerVersion,
    -- * GHC
    ghcName,
    parseGHCPkgVersions,
    fetchGHCPkgVersions
  ) where

import Control.Applicative ((<|>), (<$>), some)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Generics (Generic)

import Staversion.Internal.HTTP (Manager, fetchURL)
import Staversion.Internal.Query (PackageName)
import Staversion.Internal.Version (Version, versionNumbers, mkVersion)
import Staversion.Internal.BuildPlan.BuildPlanMap (BuildPlanMap,  HasVersions(..))
import Staversion.Internal.BuildPlan.Parser (parserVersion)
import qualified Staversion.Internal.BuildPlan.BuildPlanMap as BuildPlanMap
import qualified Staversion.Internal.Megaparsec as P

-- | Name of a compiler
type CompilerName = Text

-- | Version of a compiler
data CompilerVersion =
  CVHead -- ^ the HEAD version
  | CVNumbered Version -- ^ a numbered version.
  deriving (Show,Eq,Ord,Generic)

-- | Make a 'CVNumbered' "CompilerVersion".
mkCompilerVersion :: [Int] -> CompilerVersion
mkCompilerVersion = CVNumbered . mkVersion

instance Hashable CompilerVersion where
  hashWithSalt s CVHead = hashWithSalt s ()
  hashWithSalt s (CVNumbered v) = hashWithSalt s $ versionNumbers v

-- | A compiler with an explicit version.
data Compiler =
  Compiler
  { compilerName :: CompilerName,
    compilerVersion :: CompilerVersion
  }
  deriving (Show,Eq,Ord,Generic)

instance Hashable Compiler

-- | Build plan of the core packages for a compiler.
data CoreBuildPlanMap =
  CoreBuildPlanMap
  { coreCompiler :: Compiler,
    coreMap :: BuildPlanMap
  }
  deriving (Show,Eq)

instance HasVersions CoreBuildPlanMap where
  packageVersion cbp = packageVersion $ coreMap cbp

-- | Name of ghc.
ghcName :: CompilerName
ghcName = "ghc"

-- | Compilers and its corresponding core packages.
type CompilerCores = HM.HashMap Compiler CoreBuildPlanMap

addVersions :: Compiler -> [(PackageName, Version)] -> CompilerCores -> CompilerCores
addVersions c pkgs = HM.insertWith merge c inserted_cbp
  where
    inserted_cbp = CoreBuildPlanMap c $ BuildPlanMap.fromList pkgs
    merge new old = new { coreMap = coreMap new <> coreMap old }

parsePkgVersionsLine :: Text -> Either String (Compiler, [(PackageName, Version)])
parsePkgVersionsLine input = mapError $ P.runParser parser "" input
  where
    mapError (Left e) = Left $ show e
    mapError (Right a) = Right a
    parser = do
      P.space
      cv <- parserCompilerVersion
      P.space1
      vers <- P.sepBy parserPVer P.space1
      return (Compiler ghcName cv, vers)
    parserCompilerVersion = headVersion <|> (CVNumbered <$> parserVersion)
    headVersion = do
      void $ P.string "HEAD"
      return CVHead
    parserPVer = do
      name <- P.textSatisfying (\c -> c /= '/')
      void $ P.char '/'
      ver <- parserVersion
      return (name, ver)

-- | Parse the \"pkg_versions.txt\" file for GHC core packages.
parseGHCPkgVersions :: BSL.ByteString -> Either String (HM.HashMap Compiler CoreBuildPlanMap)
parseGHCPkgVersions content =
  foldlM f HM.empty $ filter (not . isWhiteLine) $ map removeComment $ toLines content
  where
    toLines :: BSL.ByteString -> [Text]
    toLines = map TL.toStrict . TL.lines . TL.decodeUtf8
    removeComment = T.takeWhile (\c -> c /= '#')
    isWhiteLine = T.all isSpace
    f acc line = do
      (c, vers) <- parsePkgVersionsLine line
      return $ addVersions c vers acc

-- | Fetch the \"pkg_versions.txt\" from the Web.
fetchGHCPkgVersions :: Manager -> IO BSL.ByteString
fetchGHCPkgVersions man = fetchURL man "https://gitlab.haskell.org/bgamari/ghc-utils/-/raw/master/library-versions/pkg_versions.txt"
