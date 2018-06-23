-- |
-- Module: Staversion.Internal.BuildPlan.StackYaml
-- Description: Get PackageSource from stack.yaml
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
--
-- This module is meant to be exposed only to
-- "Staversion.Internal.BuildPlan" and test modules.
module Staversion.Internal.BuildPlan.StackYaml
       ( readResolver
       ) where

import Control.Applicative (empty)
import Data.Yaml (FromJSON(..), Value(..), (.:), decodeEither)
import qualified Data.ByteString as BS

import Staversion.Internal.Query (Resolver, ErrorMsg)

newtype Resolver' = Resolver' { unResolver' :: Resolver }
                  deriving (Show,Eq,Ord)

instance FromJSON Resolver' where
  parseJSON (Object o) = fmap Resolver' $ o .: "resolver"
  parseJSON _ = empty

readResolver :: FilePath -- ^ path to stack.yaml
             -> IO (Either ErrorMsg Resolver)
readResolver file = fmap (fmap unResolver' . decodeEither) $ BS.readFile file
