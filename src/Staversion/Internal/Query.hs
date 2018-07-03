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
         ErrorMsg
       ) where

import Data.List (isSuffixOf)
import Data.Text (Text, pack)

type PackageName = Text

-- | Resolver name at stackage like "lts-4.1".
type Resolver = String

-- | Source of packages.
data PackageSource = SourceStackage Resolver -- ^ stackage.
                   | SourceHackage -- ^ hackage (latest)
                   | SourceStackYaml FilePath
                     -- ^ stack.yaml file. Its \"resolver\" field is
                     -- used as the package source.
                   | SourceStackDefault
                     -- ^ the resolver that the @stack@ command would
                     -- use by default.
                   deriving (Show,Eq,Ord)

-- | Query for package version(s).
data Query = QueryName PackageName
           | QueryCabalFile FilePath
           | QueryStackYaml FilePath
           | QueryStackYamlDefault
           deriving (Show,Eq,Ord)

type ErrorMsg = String

-- | description of a 'PackageSource'.
sourceDesc :: PackageSource -> Text
sourceDesc (SourceStackage r) = pack r
sourceDesc SourceHackage = "latest in hackage"
sourceDesc (SourceStackYaml p) = pack p
sourceDesc SourceStackDefault = "default stack resolver"

parseQuery :: String -> Query
parseQuery s = if ".cabal" `isSuffixOf` s
               then QueryCabalFile s
               else QueryName $ pack s
