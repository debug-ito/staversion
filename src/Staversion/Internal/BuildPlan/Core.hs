-- |
-- Module: Staversion.Internal.BuildPlan.Core
-- Description: Build plan of core packages (those bundled with a compiler)
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.BuildPlan.Core
  ( -- * Types
    CoreBuildPlan(..),
    Compiler(..),
    CompilerName,
    -- * GHC
    ghcName,
    parseGHCPkgVersions,
    fetchGHCPkgVersions
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Staversion.Internal.HTTP (Manager)
import Staversion.Internal.Version (Version)
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

-- | Build plan of the core packages for a compiler.
data CoreBuildPlan =
  CoreBuildPlan
  { cbpCompiler :: Compiler,
    cbpMap :: BuildPlanMap
  }
  deriving (Show,Eq)

instance HasVersions CoreBuildPlan where
  packageVersion cbp = packageVersion $ cbpMap cbp

-- | Name of ghc.
ghcName :: CompilerName
ghcName = "ghc"

-- | Parse the \"pkg_versions.txt\" file for GHC core packages.
parseGHCPkgVersions :: BSL.ByteString -> Either String (HM.HashMap Compiler CoreBuildPlan)
parseGHCPkgVersions = undefined -- TODO.

-- | Fetch the \"pkg_versions.txt\" from the Web.
fetchGHCPkgVersions :: Manager -> IO BSL.ByteString
fetchGHCPkgVersions = undefined -- TODO.

