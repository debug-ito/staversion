-- |
-- Module: Staversion.Internal.BuildPlan.Pantry
-- Description: Pantry YAML format of build plan
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Staversion.Internal.BuildPlan.Pantry
  ( PantryBuildPlanMap,
    toBuildPlanMap,
    parseBuildPlanMapYAML,
    fetchBuildPlanMapYAML
  ) where

import Control.Monad (void)
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Text (pack)

import Staversion.Internal.BuildPlan.BuildPlanMap
  ( BuildPlanMap,
    HasVersions(..)
  )
import Staversion.Internal.BuildPlan.Core
  ( Compiler,
    CoreBuildPlanMap(..)
  )
import Staversion.Internal.BuildPlan.Parser (parserVersion, manyTillWithEnd)
import Staversion.Internal.BuildPlan.Stackage (ExactResolver(..))
import qualified Staversion.Internal.Megaparsec as P
import Staversion.Internal.HTTP (Manager)
import Staversion.Internal.Query (ErrorMsg, PackageName)
import Staversion.Internal.Version (Version)

-- | A build plan map loaded from a Pantry YAML file. This is not a
-- complete 'BuildPlanMap', because it implicitly refers to
-- 'CoreBuildPlanMap'. That's why its data constructor is not
-- exported.
data PantryBuildPlanMap =
  PantryBuildPlanMap
  { pantryCompiler :: Compiler,
    pantryMap :: BuildPlanMap
  }

instance HasVersions PantryBuildPlanMap where
  packageVersion pbp = packageVersion (pantryMap pbp)

-- | Combine 'PantryBuildPlanMap' and 'CoreBuildPlanMap' to make a
-- complete 'BuildPlanMap'.
toBuildPlanMap :: CoreBuildPlanMap -> PantryBuildPlanMap -> Either String BuildPlanMap
toBuildPlanMap cbp pbp = 
  if ccv == pcv
  then Right $ pantryMap pbp <> coreMap cbp
  else Left ("Unmatched compiler versions: Core: " <> show ccv <> ", Pantry: " <> show pcv)
  where
    ccv = coreCompiler cbp
    pcv = pantryCompiler pbp

-- | Parse a YAML document for a 'CoreBuildPlanMap'.
parseBuildPlanMapYAML :: BSL.ByteString -> Either ErrorMsg CoreBuildPlanMap
parseBuildPlanMapYAML = undefined -- TODO

-- | Fetch a Pantry build plan file from the Web.
fetchBuildPlanMapYAML :: Manager -> ExactResolver -> IO BSL.ByteString
fetchBuildPlanMapYAML = undefined -- TODO

parserPackage :: P.Parser () -- ^ Parser of a symbol that follows the packageName-version string.
              -> P.Parser (PackageName, Version)
parserPackage end = do
  (pstr, ver) <- manyTillWithEnd P.anyChar versionAndEnd
  return (pack pstr, ver)
  where
    versionAndEnd = do
      void $ P.char '-'
      v <- parserVersion
      end
      return v
