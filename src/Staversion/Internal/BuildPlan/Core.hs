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

import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics (Generic)

import Staversion.Internal.HTTP (Manager)
import Staversion.Internal.Version (Version, versionNumbers, mkVersion)
import Staversion.Internal.BuildPlan.BuildPlanMap (BuildPlanMap,  HasVersions(..))

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

-- instance Hashable Compiler where
--   hashWithSalt s c =
--     s `hashWithSalt` (compilerName c) `hashWithSalt` hashable_version
--     where
--       hashable_version = versionNumbers $ compilerVersion c

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

-- | Parse the \"pkg_versions.txt\" file for GHC core packages.
parseGHCPkgVersions :: BSL.ByteString -> Either String (HM.HashMap Compiler CoreBuildPlanMap)
parseGHCPkgVersions = undefined -- TODO.

-- | Fetch the \"pkg_versions.txt\" from the Web.
fetchGHCPkgVersions :: Manager -> IO BSL.ByteString
fetchGHCPkgVersions = undefined -- TODO.

