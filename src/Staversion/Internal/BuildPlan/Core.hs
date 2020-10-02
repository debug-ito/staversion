-- |
-- Module: Staversion.Internal.BuildPlan.Core
-- Description: Build plan of core packages (those bundled with a compiler)
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.BuildPlan.Core
  ( -- * Types
    CoreBuildPlanMap(..),
    Compiler(..),
    CompilerName,
    -- * GHC
    ghcName,
    parseGHCPkgVersions,
    fetchGHCPkgVersions
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Staversion.Internal.HTTP (Manager)
import Staversion.Internal.Version (Version, versionNumbers)
import Staversion.Internal.BuildPlan.BuildPlanMap (BuildPlanMap,  HasVersions(..))

-- | Name of a compiler
type CompilerName = Text

-- | A compiler version.
data Compiler =
  Compiler
  { compilerName :: CompilerName,
    compilerVersion :: Version
  }
  deriving (Show,Eq,Ord)

instance Hashable Compiler where
  hashWithSalt s c =
    s `hashWithSalt` (compilerName c) `hashWithSalt` hashable_version
    where
      hashable_version = versionNumbers $ compilerVersion c

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

