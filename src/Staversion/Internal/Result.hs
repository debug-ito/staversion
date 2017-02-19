-- |
-- Module: Staversion.Internal.Result
-- Description: Result data type and its utilities
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Result
       ( Result(..),
         ResultSource(..),
         ResultBody,
         ResultBody'(..)
       ) where

import Data.Version (Version)
import Staversion.Internal.Query
  ( Query, PackageSource, ErrorMsg, PackageName
  )
import Staversion.Internal.Cabal (Target)

-- | Result for a query.
data Result = Result { resultIn :: ResultSource,
                       resultFor :: Query,
                       resultBody :: Either ErrorMsg ResultBody
                     } deriving (Show,Eq,Ord)

data ResultSource =
  ResultSource { resultSourceQueried :: PackageSource,
                 -- ^ the 'PackageSource' queried by user.
                 resultSourceReal :: Maybe PackageSource
                 -- ^ the real (exact) 'PackageSource' resolved.
               } deriving (Show,Eq,Ord)

type ResultBody = ResultBody' (Maybe Version)

data ResultBody' a = SimpleResultBody PackageName a
                   | CabalResultBody FilePath Target [(PackageName, a)]
                   deriving (Show,Eq,Ord)
