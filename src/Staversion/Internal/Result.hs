-- |
-- Module: Staversion.Internal.Result
-- Description: Result data type and its utilities
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Result
       ( Result(..),
         ResultBody(..)
       ) where

import Data.Version (Version)
import Staversion.Internal.Query
  ( Query, PackageSource, ErrorMsg, PackageName
  )
import Staversion.Internal.Cabal (Target)

-- | Result for a query.
data Result = Result { resultIn :: PackageSource,
                       resultFor :: Query,
                       resultReallyIn :: Maybe PackageSource,
                       -- ^ the true PackageSource resolved (or redirected) from 'resultIn', if any.
                       resultBody :: Either ErrorMsg ResultBody
                     } deriving (Show,Eq)

data ResultBody = SimpleResultBody PackageName (Maybe Version)
                | CabalResultBody FilePath Target [(PackageName, (Maybe Version))]
                deriving (Show,Eq)
