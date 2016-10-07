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
         Query(..),
         ErrorMsg,
         Result(..)
       ) where

import Data.Text (Text)
import Data.Version (Version)

type PackageName = Text

-- | Resolver name at stackage like "lts-4.1".
type Resolver = String

-- | Source of packages.
data PackageSource = SourceStackage Resolver -- ^ stackage.
                   deriving (Show,Eq,Ord)

-- | Query for package version(s).
data Query = QueryName PackageName
           deriving (Show,Eq,Ord)

type ErrorMsg = String

-- | Result for a query.
data Result = Result { resultIn :: PackageSource,
                       resultFor :: Query,
                       resultVersion :: Either ErrorMsg Version
                     } deriving (Show,Eq,Ord)
