-- |
-- Module: Staversion.Internal.Query
-- Description: Query, Result and related symbols.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Query
       ( PackageName,
         Resolver,
         PackageSource(..),
         sourceDesc,
         Query(..),
         parseQuery,
         ErrorMsg,
         Result(..),
         ResultBody(..)
       ) where

import qualified Data.HashMap.Strict as HM
import Data.List (isSuffixOf)
import Data.Text (Text, pack)
import Data.Version (Version)

type PackageName = Text

-- | Resolver name at stackage like "lts-4.1".
type Resolver = String

-- | Source of packages.
data PackageSource = SourceStackage Resolver -- ^ stackage.
                   | SourceHackage -- ^ hackage (latest)
                   deriving (Show,Eq,Ord)

-- | Query for package version(s).
data Query = QueryName PackageName
           | QueryCabalFile FilePath
           deriving (Show,Eq,Ord)

type ErrorMsg = String

-- | Result for a query.
data Result = Result { resultIn :: PackageSource,
                       resultFor :: Query,
                       resultReallyIn :: Maybe PackageSource,
                       -- ^ the true PackageSource resolved (or redirected) from 'resultIn', if any.
                       resultBody :: Either ErrorMsg ResultBody
                     } deriving (Show,Eq)

data ResultBody = SimpleResultBody PackageName (Maybe Version)
                  deriving (Show,Eq)

-- | The obtained version map.
newtype ResultVersions = ResultVersions (HM.HashMap PackageName (Maybe Version))
                       deriving (Show,Eq)

resultVersionsFromList :: [(PackageName, Maybe Version)] -> ResultVersions
resultVersionsFromList = ResultVersions . HM.fromList

resultVersionsToList :: ResultVersions -> [(PackageName, Maybe Version)]
resultVersionsToList (ResultVersions m) = HM.toList m

-- | description of a 'PackageSource'.
sourceDesc :: PackageSource -> Text
sourceDesc (SourceStackage r) = pack r
sourceDesc SourceHackage = "latest in hackage"

parseQuery :: String -> Query
parseQuery s = if ".cabal" `isSuffixOf` s
               then QueryCabalFile s
               else QueryName $ pack s
