-- |
-- Module: Staversion.Internal.Query
-- Description:  
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Query
       ( PackageName,
         Resolver,
         PackageSource(..),
         Query(..),
         Result(..)
       ) where

import Data.Text (Text)
import Data.Version (Version)

type PackageName = Text

-- | Resolver name at stackage like "lts-4.1".
type Resolver = Text

-- | Source of packages.
data PackageSource = SourceStackage Resolver -- ^ stackage.
                   deriving (Show,Eq,Ord)

-- | Query for package version(s).
data Query = QueryName { querySource :: PackageSource,
                         queryName :: PackageName
                       } deriving (Show,Eq,Ord)

-- | Result for a query.
data Result = Result { resultFor :: Query,
                       resultVersion :: Maybe Version
                     } deriving (Show,Eq,Ord)
