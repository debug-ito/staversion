-- |
-- Module: Staversion.Internal.Format
-- Description: formatting Result output.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module Staversion.Internal.Format
       ( formatResultsCabal,
         formatResultsCabalAggregated
       ) where

import Data.Foldable (fold)
import Data.Function (on)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString)
import Distribution.Version (VersionRange)

import Staversion.Internal.Aggregate (Aggregator, showVersionRange)
import Staversion.Internal.Query
  ( Query(..),
    sourceDesc,
    PackageName
  )
import Staversion.Internal.Result
  ( Result(..), ResultBody'(..), ResultSource(..),
    AggregatedResult(..), singletonResult
  )
import Staversion.Internal.Cabal (Target(..))

-- | format 'Result's like it's in build-depends in .cabal files.
formatResultsCabal :: [Result] -> TL.Text
formatResultsCabal = toLazyText . mconcat . map formatResultBlock
                     . makeSourceBlocks . map singletonResult

-- | aggregate 'Result's and format them like it's in build-depends in
-- .cabal files.
formatResultsCabalAggregated :: Aggregator -> [Result] -> TL.Text
formatResultsCabalAggregated = undefined

groupAllPreservingOrderBy :: (a -> a -> Bool)
                             -- ^ The comparator that determines if the two elements are in the same group.
                             -- This comparator must be transitive, like '(==)'.
                          -> [a] -> [NonEmpty a]
groupAllPreservingOrderBy sameGroup = foldr f [] where
  f item acc = update [] acc where
    update heads [] = (item :| []) : heads
    update heads (cur@(cur_head :| cur_rest) : rest) =
      if sameGroup item cur_head
      then ((item :| (cur_head : cur_rest)) : heads) ++ rest 
      else update (heads ++ [cur]) rest

-- | 'Left' lines and 'Right' lines are handled differently by
-- 'formatResultBlock'. It puts commas at the right places assuming
-- 'Left' lines are commented out.
type ResultLine = Either Builder Builder

data ResultBlock = RBHead Builder [ResultBlock] -- ^ header and child blocks
                 | RBLines [ResultLine] -- ^ a block, which consists of some lines.

makeSourceBlocks :: [AggregatedResult] -> [ResultBlock]
makeSourceBlocks = map sourceBlock . groupAllPreservingOrderBy ((==) `on` aggResultIn) where
  sourceBlock results@(head_ret :| _) = RBHead header $ makeQueryBlocks $ NL.toList results where
    header = "------ " <> (fold $ fmap sourceHeader $ aggResultIn head_ret)

sourceHeader :: ResultSource -> Builder
sourceHeader src = query_source <> real_source where
  query_source = fromText $ sourceDesc $ resultSourceQueried $ src
  real_source = case resultSourceReal src of
    Nothing -> ""
    Just real_psource -> " (" <> (fromText $ sourceDesc real_psource) <> ")"

makeQueryBlocks :: [AggregatedResult] -> [ResultBlock]
makeQueryBlocks = uncurry prependLines . foldr f ([], []) where
  prependLines blocks [] = blocks
  prependLines blocks rlines = (RBLines rlines) : blocks
  f ret (blocks, rlines) = case (aggResultFor ret, aggResultBody ret) of
    (_, Right (SimpleResultBody name mver)) -> (blocks, (versionLine name mver) : rlines)
    (_, Right (CabalResultBody file target pairs)) -> (cabalFileSuccessBlock file target pairs : prependLines blocks rlines, [])
    ((QueryName name), Left _) -> (blocks, (packageErrorLine name) : rlines)
    ((QueryCabalFile file), Left _) -> (cabalFileErrorBlock file : prependLines blocks rlines, [])

versionLine :: PackageName -> Maybe VersionRange -> ResultLine
versionLine name Nothing = Left $ "-- " <> fromText name <> " N/A"
versionLine name (Just ver_range) = Right $ fromText name <> " " <> (fromString $ showVersionRange ver_range)

packageErrorLine :: PackageName -> ResultLine
packageErrorLine name = Left $ "-- " <> fromText name <> " ERROR"

cabalFileErrorBlock :: FilePath -> ResultBlock
cabalFileErrorBlock file = RBLines [Left line] where
  line = "-- " <> fromString file <> " ERROR"

cabalFileSuccessBlock :: FilePath -> Target -> [(PackageName, Maybe VersionRange)] -> ResultBlock
cabalFileSuccessBlock file target pairs = RBHead header [RBLines $ map (uncurry versionLine) pairs] where
  header = "-- " <> fromString file <> " - " <> target_text
  target_text = case target of
    TargetLibrary -> "library"
    TargetExecutable n -> "executable " <> fromText n
    TargetTestSuite n -> "test-suite " <> fromText n
    TargetBenchmark n -> "benchmark " <> fromText n

formatResultBlock :: ResultBlock -> Builder
formatResultBlock (RBHead header blocks) = header <> "\n" <> mconcat (map formatResultBlock blocks)
formatResultBlock (RBLines rlines) = (mconcat $ map ((<> "\n") . either id id) $ tailCommas rlines) <> "\n" where
  tailCommas = fst . foldr f ([], False)
               -- flag: True if it has already encountered the last Right element in the list.
  f eb (ret, flag) = let (next_e, next_flag) = getNext ret flag eb
                     in (next_e:ret, next_flag)
  getNext [] flag e@(Left _) = (e, flag)
  getNext _ flag (Left b) = (Left (b <> ","), flag)
  getNext _ False e@(Right _) = (e, True)
  getNext _ True (Right b) = (Right (b <> ","), True)
