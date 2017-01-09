-- |
-- Module: Staversion.Internal.Aggregate
-- Description: aggregation of multiple versions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Aggregate
       ( Aggregator,
         VersionRange,
         showVersionRange,
         aggOr
       ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Version (Version)
import Distribution.Version (VersionRange)
import qualified Distribution.Version as V
import qualified Distribution.Text as DT
import qualified Text.PrettyPrint as Pretty

-- | Aggregate some 'Version's into a 'VersionRange'.
type Aggregator = NonEmpty Version -> VersionRange

showVersionRange :: VersionRange -> String
showVersionRange = Pretty.render . DT.disp

-- | Aggregator of ORed versions.
aggOr :: Aggregator
aggOr vs = foldr f (V.thisVersion $ NL.last svs) $ NL.init svs where
  svs = NL.sort vs
  f elem_v range = V.unionVersionRanges (V.thisVersion elem_v) range

