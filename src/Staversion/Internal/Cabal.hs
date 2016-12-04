-- |
-- Module: Staversion.Internal.Cabal
-- Description: functions dealing with .cabal files.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Cabal
       ( loadCabalFile,
         Target(..),
         BuildDepends(..)
       ) where

import Staversion.Internal.Query
  ( PackageName, ErrorMsg
  )

-- | Build target type.
data Target = TargetLibrary -- ^ the @library@ target.
            | TargetExecutable String -- ^ the @executable NAME@ target.
            | TargetTestSuite String -- ^ the @test-suite NAME@ target.
            | TargetBenchMark String -- ^ the @benchmark NAME@ target.
            deriving (Show,Eq,Ord)

-- | A block of @build-depends:@.
data BuildDepends =
  BuildDepends { depsTarget :: Target,
                 depsPackages :: [PackageName]
               } deriving (Show,Eq,Ord)

loadCabalFile :: FilePath -> IO (Either ErrorMsg BuildDepends)
loadCabalFile = undefined

