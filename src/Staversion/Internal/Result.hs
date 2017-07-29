-- |
-- Module: Staversion.Internal.Result
-- Description: Result data type and its utilities
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Result
       ( Result(..),
         ResultSource(..),
         resultSourceDesc,
         ResultBody,
         ResultBody'(..),
         AggregatedResult(..),
         singletonResult
       ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import Staversion.Internal.Query
  ( Query, PackageSource, ErrorMsg, PackageName,
    sourceDesc
  )
import Staversion.Internal.Cabal (Target)
import Staversion.Internal.Version (Version, VersionRange, thisVersion)

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

resultSourceDesc :: ResultSource -> Text
resultSourceDesc src = query_source <> real_source where
  query_source = sourceDesc $ resultSourceQueried $ src
  real_source = case resultSourceReal src of
    Nothing -> ""
    Just real_psource -> if real_psource == resultSourceQueried src
                         then ""
                         else " (" <> sourceDesc real_psource <> ")"

-- | For backward-compatibility.
type ResultBody = ResultBody' (Maybe Version)

data ResultBody' a = SimpleResultBody PackageName a
                   | CabalResultBody FilePath Target [(PackageName, a)]
                   deriving (Show,Eq,Ord)

instance Functor ResultBody' where
  fmap f (SimpleResultBody n a) = SimpleResultBody n (f a)
  fmap f (CabalResultBody fp t pairs) = CabalResultBody fp t (map (\(n, a) -> (n, f a)) pairs)

-- | Results for a query aggregated over different sources.
data AggregatedResult =
  AggregatedResult { aggResultIn :: NonEmpty ResultSource,
                     aggResultFor :: Query,
                     aggResultBody :: Either ErrorMsg (ResultBody' (Maybe VersionRange))
                   } deriving (Show,Eq)

-- | Create an 'AggregatedResult' that includes just one 'Result'.
singletonResult :: Result -> AggregatedResult
singletonResult ret = AggregatedResult { aggResultIn = (resultIn ret :| []),
                                         aggResultFor = resultFor ret,
                                         aggResultBody = (fmap . fmap . fmap) thisVersion $ resultBody ret
                                       }
