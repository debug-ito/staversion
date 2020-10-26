-- |
-- Module: Staversion.Internal.BuildPlan.Pantry
-- Description: Pantry YAML format of build plan
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- @since 0.2.4.0
module Staversion.Internal.BuildPlan.Pantry
  ( PantryBuildPlanMap,
    PantryName,
    pantryCompiler,
    pantryName,
    toBuildPlanMap,
    coresToBuildPlanMap,
    parseBuildPlanMapYAML,
    fetchBuildPlanMapYAML
  ) where

import Control.Applicative ((<$>), (<*>), empty, (<|>))
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:!))
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import qualified Data.Yaml as Yaml

import Staversion.Internal.BuildPlan.BuildPlanMap
  ( BuildPlanMap,
    HasVersions(..),
  )
import qualified Staversion.Internal.BuildPlan.BuildPlanMap as BuildPlanMap
import Staversion.Internal.BuildPlan.Core
  ( Compiler(..),
    CoreBuildPlanMap(..),
    CompilerVersion(..),
    CompilerCores
  )
import Staversion.Internal.BuildPlan.Parser (parserVersion, manyTillWithEnd)
import Staversion.Internal.BuildPlan.Stackage (ExactResolver(..))
import qualified Staversion.Internal.Megaparsec as P
import Staversion.Internal.HTTP (Manager, fetchURL)
import Staversion.Internal.Query (ErrorMsg, PackageName)
import Staversion.Internal.Version (Version)

-- | Name of a pantry snapshot
type PantryName = Text

-- | A build plan map loaded from a Pantry YAML file. This is not a
-- complete 'BuildPlanMap', because it implicitly refers to
-- 'CoreBuildPlanMap'. That's why its data constructor is not
-- exported.
data PantryBuildPlanMap =
  PantryBuildPlanMap
  { pantryName :: Maybe PantryName,
    pantryCompiler :: Compiler,
    pantryMap :: BuildPlanMap
  }

instance HasVersions PantryBuildPlanMap where
  packageVersion pbp = packageVersion (pantryMap pbp)

instance FromJSON PantryBuildPlanMap where
  parseJSON (Object o) =
    PantryBuildPlanMap
    <$> (o .:! "name")
    <*> fmap unPantryCompiler parserCompiler
    <*> fmap fromPantryPackageList (o .: "packages")
    where
      parserCompiler = (o .: "compiler") <|> parserResolverCompiler
      parserResolverCompiler = do
        res <- o .: "resolver"
        res .: "compiler"
  parseJSON _ = empty

-- | Internal type to parse a package in Pantry YAML.
newtype PantryPackage = PantryPackage { unPantryPackage :: (PackageName, Version) }
  deriving (Show,Eq,Ord)

fromPantryPackageList :: [PantryPackage] -> BuildPlanMap
fromPantryPackageList = BuildPlanMap.fromList . map unPantryPackage

instance FromJSON PantryPackage where
  parseJSON (Object o) = fmap PantryPackage $ parsePText =<< (o .: "hackage")
    where
      parsePText :: Text -> Aeson.Parser (PackageName, Version)
      parsePText t = either (fail . show) return $ P.runParser the_parser "" t
      the_parser = parserPackage (void $ P.char '@')
  parseJSON _ = empty

-- | Internal type to parse a compiler in Pantry YAML.
newtype PantryCompiler = PantryCompiler { unPantryCompiler :: Compiler }
  deriving (Show,Eq,Ord)

instance FromJSON PantryCompiler where
  parseJSON (String s) = fmap toCompiler $ either (fail . show) return $ P.runParser the_parser "" s
    where
      the_parser = parserPackage (P.eof)
      toCompiler (name, ver) = PantryCompiler $ Compiler name $ CVNumbered ver
  parseJSON _ = empty

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

-- | Select a 'CoreBuildPlanMap' from the given map to make a complete
-- 'BuildPlanMap' from 'PantryBuildPlanMap'.
coresToBuildPlanMap :: CompilerCores -> PantryBuildPlanMap -> Either String BuildPlanMap
coresToBuildPlanMap cmap pbp = do
  cbp <- maybe (Left ("No CoreBuildPlanMap for compiler " ++ show compiler)) Right $ HM.lookup compiler cmap
  toBuildPlanMap cbp pbp
  where
    compiler = pantryCompiler pbp

-- | Parse a YAML document for a 'CoreBuildPlanMap'.
parseBuildPlanMapYAML :: BS.ByteString -> Either ErrorMsg PantryBuildPlanMap
parseBuildPlanMapYAML = either (Left . toErrorMsg) Right . Yaml.decodeEither'
  where
    toErrorMsg e = "Error while parsing PantryBuildPlanMap: " ++ show e

-- | Fetch a Pantry build plan file from the Web.
fetchBuildPlanMapYAML :: Manager -> ExactResolver -> IO BSL.ByteString
fetchBuildPlanMapYAML man er = fetchURL man url
  where
    url = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/" <> resolver_part
    resolver_part =
      case er of
        ExactLTS major minor -> "lts/" ++ show major ++ "/" ++ show minor ++ ".yaml"
        ExactNightly year month day -> "nightly/" ++ show year ++ "/" ++ show month ++ "/" ++ show day ++ ".yaml"

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
